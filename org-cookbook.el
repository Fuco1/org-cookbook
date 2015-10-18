;;; org-cookbook.el --- Manage your recipes with org! -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 17th October 2015
;; Package-requires: ((dash "2.10.0"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)

(require 'org)
(require 'calc)
(require 'calc-units)

(defun org-cookbook--is-primary-p ()
  "Test if the recipe is a primary ingredient.

A primary ingredient is not composed of any other ingredient of
recipe."
  (save-excursion
    (org-back-to-heading)
    ;; TODO: abstract this search, it is replicated verbatim below
    (not (re-search-forward "^|-" (save-excursion (org-end-of-subtree t)) t))))

(defun org-cookbook--goto-ingredient-table ()
  "Go to ingredient table if compound recipe or do nothing.

Returns nil for primary recipes, otherwise positions the point at
start of the ingredient table and returns position."
  (let (table)
    (save-excursion
      (org-back-to-heading)
      (setq table (re-search-forward "^|-" (save-excursion (org-end-of-subtree t)) t)))
    (when table
      (goto-char table))))

;; TODO: Cache?
(defun org-cookbook--get-recipe (recipe)
  "Return position in the buffer where RECIPE is stored."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^\\*+ +" (regexp-quote recipe)))
      (beginning-of-line)
      (point))))

(cl-defstruct org-cookbook-amount amount unit)

(defun org-cookbook--parse-amount (amount)
  "Parse AMOUNT.

Amount is a number (possibly with . or , as decimal separator)
followed by a unit."
  (save-match-data
    (string-match "\\([0-9]+\\(?:[,.][0-9]+\\)?\\)[[:space:]]*\\(.*\\)" amount)
    (make-org-cookbook-amount
     :amount (string-to-number (replace-regexp-in-string "," "." (match-string 1 amount)))
     :unit (match-string 2 amount))))

(defun org-cookbook--convert-amount (amount target-units)
  "Convert AMOUNT to TARGET-UNITS."
  (let ((calc-line-numbering nil))
    (math-format-stack-value (list (math-normalize
                                    (math-convert-units
                                     (math-read-expr amount)
                                     (math-read-expr target-units)))
                                   1 nil))))

;; TODO: add fibre
(cl-defstruct org-cookbook-recipe unit calories fats carbohydrates proteins cost)

(defun org-cookbook--get-properties ()
  "Get properties of recipe at point."
  (let ((props (org-entry-properties)))
    (make-org-cookbook-recipe
     :unit (cdr (assoc "UNIT" props))
     :proteins (string-to-number (cdr (assoc "PROTEINS" props)))
     :carbohydrates (string-to-number (cdr (assoc "CARBOHYDRATES" props)))
     :fats (string-to-number (cdr (assoc "FATS" props)))
     :calories (string-to-number (cdr (assoc "CALORIES" props)))
     :cost (or (--when-let (cdr (assoc "COST" props)) (string-to-number it)) 0))))

;; TODO: finish unit conversion
(defun org-cookbook--scale-recipe (recipe units)
  "Scale a RECIPE (or ingredient) to UNITS."
  (let ((multiple (/ (float (string-to-number units))
                     (string-to-number (org-cookbook-recipe-unit recipe)))))
    (make-org-cookbook-recipe
     :proteins (* multiple (org-cookbook-recipe-proteins recipe))
     :carbohydrates (* multiple (org-cookbook-recipe-carbohydrates recipe))
     :fats (* multiple (org-cookbook-recipe-fats recipe))
     :calories (* multiple (org-cookbook-recipe-calories recipe))
     :cost (* multiple (org-cookbook-recipe-cost recipe)))))

(defun org-cookbook--add-properties (a b)
  "Add properties of two ingredients A and B."
  (make-org-cookbook-recipe
   :proteins (+ (org-cookbook-recipe-proteins a) (org-cookbook-recipe-proteins b))
   :carbohydrates (+ (org-cookbook-recipe-carbohydrates a) (org-cookbook-recipe-carbohydrates b))
   :fats (+ (org-cookbook-recipe-fats a) (org-cookbook-recipe-fats b))
   :calories (+ (org-cookbook-recipe-calories a) (org-cookbook-recipe-calories b))
   :cost (+ (org-cookbook-recipe-cost a) (org-cookbook-recipe-cost b))))

;; If I specify UNIT in 100g or 100ml, I will usually get parts in the
;; same unit (or just scaled up or down like kg or l) => the ratio is
;; not multiplicative but divisive, e.g. 25g of 100g base should give
;; me 1/4 or 0.25 multiple

;; When I deal in pieces (for example multiplying pieces of bread or
;; carrots) I want it to be multiplicative (with the base given as 1
;; piece)

;; TODO: pass a cache through the computation to speed up
(defun org-cookbook--recompute-recipe ()
  "Recompute recipe properties."
  (save-excursion
    (if (org-cookbook--goto-ingredient-table)
        (let* ((raw-table-data (-remove-item 'hline (org-table-to-lisp)))
               (normalized-recipe (-map (-lambda ((name amount))
                                          (list
                                           name
                                           amount
                                           (org-cookbook--get-recipe name)))
                                        raw-table-data))
               ;; TODO: add some 'data structure' to hold this
               (re (make-org-cookbook-recipe :unit "1" :proteins 0 :carbohydrates 0 :fats 0 :cost 0 :calories 0)))
          (save-excursion
            (-each normalized-recipe
              (-lambda ((_name amount pos))
                (goto-char pos)
                (let* ((ingredient (org-cookbook--recompute-recipe))
                       (scaled-ingredient (org-cookbook--scale-recipe ingredient amount)))
                  (setq re (org-cookbook--add-properties re scaled-ingredient))))))
          (org-entry-put (point) "UNIT" "1")
          (org-entry-put (point) "CALORIES" (number-to-string (org-cookbook-recipe-calories re)))
          (org-entry-put (point) "FATS" (number-to-string (org-cookbook-recipe-fats re)))
          (org-entry-put (point) "CARBOHYDRATES" (number-to-string (org-cookbook-recipe-carbohydrates re)))
          (org-entry-put (point) "PROTEINS" (number-to-string (org-cookbook-recipe-proteins re)))
          (org-entry-put (point) "COST" (number-to-string (org-cookbook-recipe-cost re)))
          re)
      (org-cookbook--get-properties))))

(provide 'org-cookbook)
;;; org-cookbook.el ends here

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
    (org-back-to-heading t)
    ;; TODO: abstract this search, it is replicated verbatim below
    (not (re-search-forward "^|-" (save-excursion (org-end-of-subtree t)) t))))

(defun org-cookbook--goto-ingredient-table ()
  "Go to ingredient table if compound recipe or do nothing.

Returns nil for primary recipes, otherwise positions the point at
start of the ingredient table and returns position."
  (let (table)
    (save-excursion
      (org-back-to-heading t)
      (setq table (re-search-forward "^ *|-" (save-excursion (org-end-of-subtree t)) t)))
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

;; TODO: this is no longer necessary as we use calc to parse and
;; manipulate units
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

(defun make-org-cookbook-recipe (&rest args)
  "Make new cookbook recipe.

ARGS is a plist of keys and values we want to insert into the new
recipe.

The properties which are not in ARGS are populated according to
`org-cookbook-properties.'"
  (let ((re (make-hash-table :test 'equal)))
    (while args
      (org-cookbook-recipe-put (pop args) (pop args) re))
    (-each (org-cookbook-properties)
      (-lambda ((name . def))
        (unless (org-cookbook-recipe-get name re)
          (org-cookbook-recipe-put name def re))))
    (unless (org-cookbook-recipe-get "AMOUNT" re)
      (org-cookbook-recipe-put "AMOUNT" "1" re))
    re))

(defalias 'org-cookbook-recipe-get 'gethash)
(defalias 'org-cookbook-recipe-put 'puthash)

(defcustom org-cookbook-properties '(
                                     "PROTEINS"
                                     "CARBOHYDRATES"
                                     "FATS"
                                     "CALORIES"
                                     "FIBRE"
                                     ("COST" . 0)
                                     )
  "List of properties `org-cookbook' tracks.

A list of strings where each string is an org headline
property.

All these properties should have as values numbers.  If you put
anything else there things will break!

Each value can optionally be a cons cell where the `car' is the
name and `cdr' is a default value.  Default default value is 0."
  :group 'org-cookbook
  :type '(repeat (choice
                  (string :tag "Property")
                  (cons :tag "Property with default value"
                   (string :tag "Property name")
                   (number :tag "Default value")))))

(defun org-cookbook-properties ()
  "Return `org-cookbook-properties' with default default value.

This means that in case the value is a cons (with default value)
extract just the `car'."
  (--map (if (consp it) it (cons it 0)) org-cookbook-properties))

(defun org-cookbook-map-recipe (function &rest recipes)
  "Call FUNCTION on each recipe property.

RECIPES is a list of recipes.  FUNCTION should have arity equal
to number of recipes plus one.  The function is then called with
the first argument being the property name and the rest supplied
from each recipe in order as they are passed to the function.

Return a recipe whose each property is the result of applying
FUNCTION to respective properties of RECIPES."
  (let* ((props (org-cookbook-properties))
         (results (-map
                   (-lambda ((p))
                     (apply
                      function p
                      (--map
                       (org-cookbook-recipe-get p it)
                       recipes)))
                   props))
         (re (make-org-cookbook-recipe)))
    (-each (-zip (-map 'car props) results)
      (-lambda ((k . v)) (org-cookbook-recipe-put k v re)))
    re))

(defun org-cookbook-get-properties ()
  "Get properties of recipe at point.

Properties are retrieved according to `org-cookbook-properties'."
  (let ((re (make-org-cookbook-recipe)))
    (-each (org-cookbook-properties)
      (-lambda ((name . def))
        (org-cookbook-recipe-put
         name
         (or (--when-let (org-entry-get (point) name) (string-to-number it)) def)
         re)))
    ;; AMOUNT is a mandatory property
    (org-cookbook-recipe-put "AMOUNT" (org-entry-get (point) "AMOUNT") re)
    re))

(defun org-cookbook--convert-amount (amount target-units)
  "Convert AMOUNT to TARGET-UNITS."
  (let ((calc-line-numbering nil))
    (math-format-stack-value (list (math-normalize
                                    (math-convert-units
                                     (math-read-expr amount)
                                     (math-read-expr target-units)))
                                   1 nil))))

(defun org-cookbook--divide-amounts (divident divisor)
  "Divide DIVIDENT by DIVISOR.

DIVIDENT and DIVISOR are amounts with units."
  (let ((calc-line-numbering nil))
    (string-to-number
     (math-format-stack-value
      (list (math-simplify-units
             (math-normalize
              (math-div
               (math-read-expr divident)
               (math-read-expr divisor))))
            1 nil)))))

(defun org-cookbook-scale-recipe (recipe base)
  "Scale a RECIPE (or ingredient) to BASE.

For example, if RECIPE is a primary ingredient with 100g listing
and BASE is 25g, all the properties will be scaled by 1/4."
  (let* ((multiple (org-cookbook--divide-amounts
                    base
                    (org-cookbook-recipe-get "AMOUNT" recipe)))
         (re (org-cookbook-map-recipe
               (lambda (_ val) (* val multiple))
               recipe)))
    (org-cookbook-recipe-put "AMOUNT" base re)
    re))

(defun org-cookbook-sum-properties (a b)
  "Sum properties of two ingredients A and B."
  (org-cookbook-map-recipe (lambda (_ v w) (+ v w)) a b))

(defun org-cookbook-recompute-all ()
  "Recompute all recipes."
  (interactive)
  (let ((cache (make-hash-table)))
    (org-map-entries (lambda () (org-cookbook-recompute-recipe cache) t)))
  (message "Recomputing done."))

;; TODO: pass a cache through the computation to speed up
(defun org-cookbook-recompute-recipe (&optional cache)
  "Recompute recipe properties.

CACHE is a hash table caching sub-recipe results.  Usually it is
not necessary to supply as it is created internally
automatically."
  (unless cache (setq cache (make-hash-table)))
  (save-excursion
    (if (org-cookbook--goto-ingredient-table)
        (let* ((raw-table-data (-remove-item 'hline (org-table-to-lisp)))
               (ingredients (-map (-lambda ((name amount))
                                    (list
                                     name
                                     amount
                                     (org-cookbook--get-recipe name)))
                                  raw-table-data))
               ;; TODO: add some 'data structure' to hold this
               (re (make-org-cookbook-recipe)))
          (save-excursion
            (-each ingredients
              (-lambda ((_name amount pos))
                (goto-char pos)
                (let* ((ingredient (gethash pos cache (org-cookbook-recompute-recipe cache)))
                       (scaled-ingredient (org-cookbook-scale-recipe ingredient amount)))
                  (puthash pos ingredient cache)
                  (setq re (org-cookbook-sum-properties re scaled-ingredient))))))
          (org-entry-put (point) "AMOUNT" "1")
          (org-cookbook-map-recipe
           (lambda (name value)
             (org-entry-put (point) name (format "%.3f" value)))
           re)
          re)
      (puthash
       (save-excursion
         (org-back-to-heading t)
         (point))
       (org-cookbook-get-properties)
       cache))))

(provide 'org-cookbook)
;;; org-cookbook.el ends here

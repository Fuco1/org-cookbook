(require 'ert)
(require 'f)
(require 'dash)

(let ((dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path dir))
(require 'org-cookbook)

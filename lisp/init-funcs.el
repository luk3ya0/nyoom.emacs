(require 'init-const)

(defun childframe-workable-p ()
  "Test whether childframe is workable."
  (and emacs/>=26p
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and t
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(provide 'init-funcs)

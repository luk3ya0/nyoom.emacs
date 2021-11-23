(require 'init-const)

(defun childframe-workable-p ()
  "Test whether childframe is workable."
  (and emacs/>=26p
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(provide 'init-funcs)

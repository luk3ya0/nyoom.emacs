(with-eval-after-load 'org
  (require 'org-appear)
  (require 'org-valign)
  (require 'org-fragtog)
  (require 'org-progress)
  (require 'org-inline-image)
  (require 'svg-tag)
  (require 'svg-icon)
  (require 'ox)
  (setq org-adapt-indentation nil)
  (setq valign-fancy-bar 1)
  (setq org-image-actual-width nil)
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-startup-with-latex-preview "latexpreview")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-link-frame-setup '((file . find-file)))
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.85)))

(add-hook 'org-mode-hook 'org-appear-mode)

(add-hook 'org-mode-hook 'valign-mode)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(add-hook 'org-mode-hook
          (lambda ()
	    (push 'display font-lock-extra-managed-props)
            (font-lock-add-keywords nil svg-font-lock-keywords)
            (font-lock-flush (point-min) (point-max))
	    ))

(add-hook 'org-mode-hook (lambda() (require 'org-inline-image)))

(add-hook 'org-mode-hook 'svg-tag-mode)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

(provide 'init-org)

(require 'org-appear)
(require 'org-valign)
(require 'org-fragtog)
(require 'org-progress)

(add-hook 'org-mode-hook
          (lambda ()
	    (push 'display font-lock-extra-managed-props)
            (font-lock-add-keywords nil svg-font-lock-keywords)
            (font-lock-flush (point-min) (point-max))
	    ))

(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'org-appear-mode)

(setq org-latex-create-formula-image-program 'dvisvgm)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 0.85))
(add-hook 'org-mode-hook 'org-fragtog-mode)

(add-hook 'org-mode-hook 'valign-mode)

(add-hook 'org-mode-hook (lambda() (require 'org-inline-image)))

(provide 'init-org)

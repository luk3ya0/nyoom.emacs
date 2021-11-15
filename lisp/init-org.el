(require 'org-appear)
(require 'org-valign)
(require 'org-fragtog)
(require 'org-progress)

;; (setq org-latex-create-formula-image-program 'dvisvgm)
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.85))
(with-eval-after-load 'org
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-startup-with-latex-preview "latexpreview")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-link-frame-setup '((file . find-file))))

(add-hook 'org-mode-hook
          (lambda ()
	    (push 'display font-lock-extra-managed-props)
            (font-lock-add-keywords nil svg-font-lock-keywords)
            (font-lock-flush (point-min) (point-max))
	    ))
(add-hook 'org-mode-hook 'svg-tag-mode)

(add-hook 'org-mode-hook 'org-appear-mode)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(add-hook 'org-mode-hook 'valign-mode)

(add-hook 'org-mode-hook (lambda() (require 'org-inline-image)))

(provide 'init-org)

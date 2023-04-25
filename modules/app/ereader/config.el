;;; app/ereader/config.el -*- lexical-binding: t; -*-

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :preface
  (defun nov-font-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Fira Code"
                             :height 210)
    (setq line-spacing 0.1))
  :hook (nov-mode . nov-font-setup)
  :hook (nov-mode . pangu-spacing-mode)
  :hook (nov-mode . visual-line-mode)
  ;; :hook (nov-mode . visual-fill-column-mode)
  :custom
  (nov-text-width t)
  :config
  (setq nov-text-width t)
  (setq nov-variable-pitch nil))


(use-package! calibredb
  :defer t)

(defun nov-center-images ()
            "Center images in document."
            (let* ((pixel-buffer-width (shr-pixel-buffer-width))
                   match)
              (save-excursion
                (goto-char (point-min))
                (while (setq match (text-property-search-forward
                                    'display nil
                                    (lambda (_ p) (eq (car-safe p) 'image))))
                  (when-let ((size (car (image-size
                                         (prop-match-value match) 'pixels)))
                             ((> size 150))
                             (center-pixel (floor (- pixel-buffer-width size) 2))
                             (center-pos (floor center-pixel (frame-char-width))))
                    (beginning-of-line)
                    (indent-to center-pos)
                    (end-of-line))))))

(add-hook 'nov-post-html-render-hook 'nov-center-images)

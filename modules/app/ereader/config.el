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
  ;; :hook (nov-mode . org-mode)
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

;; (advice-add #'org-display-inline-images :after #'nov-center-images)

(with-eval-after-load 'evil
  (setq sentence-end-base "[.?!…‽;][]\\n\"'”’)}»›]*")
  (require 'ov)
  (require 'org-element)
  (defun plainp ()
    "Check current sentence is paragraph and it's parent is section."
    (and
     (eq (org-element-type (org-element-at-point)) 'paragraph)
     (or
      (eq (org-element-type (org-element-property :parent (org-element-at-point))) 'section)
      (eq (org-element-type (org-element-property :parent (org-element-at-point))) 'item)
      )))

  (setq ov-map
        #s(hash-table
           size 1000
           test equal
           data (
                 "0-0" 1)))

  (defun ov-exist (beg end)
    (interactive)
    (gethash (format "%s-%s" beg end) ov-map))

  (defun current-point ()
    (interactive)
    (message (format "%s" (point))))

  (defun ov-must-rem (beg end)
    (interactive)
    (remhash (format "%s-%s" beg end) ov-map)
    (ov-clear beg end))

  (defun ov-must-put (beg end)
    (interactive)
    (puthash (format "%s-%s" beg end) 1 ov-map)
    (ov-set (ov-make beg end) 'face '(:underline "plum")))
  ;; (ov-set (ov-make beg end) 'face '(:box "plum")))

  (defun ov-map-put (beg end)
    (interactive)
    (if (ov-exist beg end)
        (ov-must-rem beg end)
      (ov-must-put beg end)))

  ;; (ov-map-put 28100 28127)
  ;; (ov-clear 28100 28127)

  (defun current-visual-sentence-end ()
    (interactive)
    (let (current-poi visual-end)
      (setq current-poi (point))
      (evil-end-of-visual-line)
      (setq visual-end (point))
      (goto-char current-poi)
      (message "%d" visual-end)
      visual-end))

  (defun next-visual-sentence-start ()
    (interactive)
    (let (current-poi visual-end)
      (setq current-poi (point))
      (evil-end-of-visual-line)
      (setq visual-end (point))
      (goto-char current-poi)
      (message "%d" visual-end)
      (+ 1 (current-visual-sentence-end))))

  (defun current-sentence-end ()
    (interactive)
    (if (bounds-of-thing-at-point 'sentence)
        (cdr (bounds-of-thing-at-point 'sentence))
      (cdr (bounds-of-thing-at-point 'line))))

  (defun current-sentence-beg ()
    (interactive)
    (if (bounds-of-thing-at-point 'sentence)
        (car (bounds-of-thing-at-point 'sentence))
      (car (bounds-of-thing-at-point 'line))))

  (defun underline-current-line-toggle ()
    (interactive)
    (if (plainp)
        (if (< (current-visual-sentence-end) (current-sentence-end))
            (progn
              (ov-map-put (current-sentence-beg) (current-visual-sentence-end))
              (ov-map-put (next-visual-sentence-start) (current-sentence-end)))
          (ov-map-put (current-sentence-beg) (current-sentence-end))))
    (if (eq major-mode 'org-mode)
        (org-latex-preview '(16)))
    )

  (defun underline-forward ()
    (interactive)
    (if (< (current-visual-sentence-end) (current-sentence-end))
        (progn
          (ov-must-rem (current-sentence-beg) (current-visual-sentence-end))
          (ov-must-rem (next-visual-sentence-start) (current-sentence-end))
          )
      (ov-must-rem (current-sentence-beg) (current-sentence-end)))
    (goto-char (+ (current-sentence-end) 2))
    (goto-char (current-sentence-beg))
    (underline-current-line-toggle))

  (defun underline-backward ()
    (interactive)
    (if (< (current-visual-sentence-end) (current-sentence-end))
        (progn
          (ov-must-rem (current-sentence-beg) (current-visual-sentence-end))
          (ov-must-rem (next-visual-sentence-start) (current-sentence-end))
          )
      (ov-must-rem (current-sentence-beg) (current-sentence-end)))
    (goto-char (- (current-sentence-beg) 2))
    (goto-char (current-sentence-beg))
    (underline-current-line-toggle))

  (define-key evil-normal-state-map (kbd "M-o") 'underline-current-line-toggle)
  (define-key evil-normal-state-map (kbd "M-n") 'underline-forward)
  (define-key evil-normal-state-map (kbd "M-p") 'underline-backward))

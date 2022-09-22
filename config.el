;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; UI
(push '(width  . 91)                         default-frame-alist)
(push '(min-width  . 1)                      default-frame-alist)
(push '(height . 54)                         default-frame-alist)
(push '(min-height . 1)                      default-frame-alist)
(push '(left-fringe    . 29)                 default-frame-alist)
(push '(right-fringe   . 29)                 default-frame-alist)
(push '(internal-border-width . 14)          default-frame-alist)
(push `(alpha . ,'(95 . 95))                 default-frame-alist)

;; (set-face-background 'default "mac:windowBackgroundColor")

;; (dolist (f (face-list)) (set-face-stipple f "alpha:30%"))

;; (setq face-remapping-alist (append face-remapping-alist '((default my/default-blurred))))

;; (defface my/default-blurred
;;    '((t :inherit 'default :stipple "alpha:30%"))
;;    "Like 'default but blurred."
;;    :group 'my)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Luke Yao"
      user-mail-address "oneTOinf@163.com")

(bind-keys ([(super a)] . mark-whole-buffer)
           ([(super c)] . kill-ring-save)
           ([(super l)] . goto-line)
           ([(super q)] . save-buffers-kill-emacs)
           ([(super s)] . save-buffer)
           ([(super v)] . yank)
           ([(super w)] . kill-this-buffer)
           ([(super z)] . undo)
           ([(super r)] . doom/reload)
           ([(super j)] . +vterm/toggle))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-unicode-font (font-spec :family "PingFang SC" :size 14))
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-smoooooth-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;;; Org Mode
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org")

(defface org-link-green
  '((t (:inherit org-link :foreground "medium sea green" :underline nil)))
  "A green link.")

(after! org
  (org-link-set-parameters "file"
                           :face 'org-link-green)
  (setq org-archive-location (concat org-directory "roam/archive.org::")
        org-display-inline-images t
        org-redisplay-inline-images t
        org-image-actual-width nil
        org-latex-default-class "ctexart"
        org-latex-compiler "xelatex"
        ;; org-startup-with-inline-images "inlineimages"
        org-startup-with-inline-images nil
        org-startup-with-latex-preview nil
        ;; org-startup-with-latex-preview "latexpreview"
        org-link-elisp-confirm-function nil
        org-link-frame-setup '((file . find-file))
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 0.9)

        org-log-done t
        org-use-property-inheritance t
        org-confirm-babel-evaluate nil
        org-list-allow-alphabetical t
        org-export-with-sub-superscripts nil
        org-export-headline-levels 5
        org-export-use-babel t
        org-use-speed-commands t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-src-preserve-indentation nil
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fold-catch-invisible-edits 'smart))

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (setq-local line-spacing 5)
            (hl-line-mode -1)  ;; for what face
            (flycheck-mode -1)))

(use-package! org-appear
  :after org
  ;; :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil))

(use-package! valign
  :after org
  :diminish
  :hook
  (org-mode . valign-mode)
  :init
  (setq valign-fancy-bar t))

(use-package! ftable
  :after org
  :diminish
  :init
  (setq ftable-fill-column 10))

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package! org-ol-tree
  :init
  (defface org-ol-tree-document-face
    '((t :family "Fira Code" :size 14 :bold nil :foreground "#59B0CF"))
    "Face used by org-ol-tree to display the root node."
    :group 'org-ol-tree-faces)

  (defface org-ol-tree-section-title-face
    '((t :inherit font-lock-doc-face :family "Fira Code" :size 14))
    "Face used by org-ol-tree to display section titles."
    :group 'org-ol-tree-faces)

  (defface org-ol-tree-section-id-face
    '((t :inherit treemacs-file-face :family "Fira Code" :size 14))
    "Face used by org-ol-tree to display section titles."
    :group 'org-ol-tree-faces)

  :config
  (setq org-ol-tree-ui-window-max-width 0.4
        org-ol-tree-ui-window-min-width 0.4
        org-ol-tree-action-move-to-target t
        org-ol-tree-ui-window-auto-resize nil)

  :commands org-ol-tree)

(map! :map org-mode-map
      :after org
      :localleader
      :desc "Outline" "O" #'org-ol-tree)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; editor > snippets
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(org-mode)))

(use-package doom-snippets
  :load-path "~/.doom.d/editors/snippets"
  :after yasnippet)


;;; Editor > Motion
(defun narrow-p ()
  "Return t if a buffer is narrowed"
   (not (equal (- (point-max) (point-min)) (buffer-size))))

(with-eval-after-load 'evil
  (defun normal-next-line()
    (interactive)
    (forward-line 1))

  (defun what-face ()
    (interactive)
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" (point)))))

  (defun normal-previous-line()
    (interactive)
    (forward-line -1))

  (defun emit-ocr ()
    (interactive)
    (insert (shell-command-to-string "/opt/homebrew/bin/ocr")))

  (defun emit-ocr-trim ()
    (interactive)
    (insert (string-trim (shell-command-to-string "/opt/homebrew/bin/ocr"))))

  (defun cycle-format ()
    (interactive)
    (org-edit-special)
    (indent-region (point-min) (point-max))
    (org-edit-src-exit)
    (save-buffer)
    ;; (evil-beginning-of-line)
    ;; (org-cycle)
    )

  (defun quickb ()
    (interactive)
    (evil-visual-char nil nil 'inclusive t)
    (evil-forward-word-end nil)
    (let ((beg (region-beginning)) (end (region-end)))
      (goto-char beg)
      (insert "*")
      (goto-char (+ end 2))
      (insert "*")))

  (defun quickv ()
    (interactive)
    (evil-visual-char nil nil 'inclusive t)
    (evil-forward-word-end nil)
    (evil-forward-char)
    (evil-forward-char)
    (let ((beg (region-beginning)) (end (region-end)))
      (goto-char beg)
      (insert "=")
      (goto-char (+ end 2))
      (insert "=")))

  (defun toggle-narrow ()
    (interactive)
    (if (narrow-p)
        (widen)
      (org-narrow-to-subtree)))

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "M-s-j") 'emit-ocr)
  (define-key evil-insert-state-map (kbd "M-s-k") 'emit-ocr-trim)
  (define-key evil-normal-state-map (kbd "s-b") 'quickb)
  (define-key evil-normal-state-map (kbd "s-=") 'quickv)
  (define-key evil-normal-state-map (kbd "s-[") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "s-]") 'next-buffer)
  (define-key evil-normal-state-map (kbd "s-f") 'cycle-format)
  (define-key evil-normal-state-map (kbd "RET") '+fold/toggle)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-o") 'toggle-narrow)
  (define-key evil-normal-state-map (kbd "s-p") 'what-face)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-define-operator wrap-with-parens (beg end)
    (goto-char beg)
    (insert "(")
    (goto-char (1+ end))
    (insert ")"))

  (evil-define-key 'visual global-map
    (kbd "(") 'wrap-with-parens)

  (evil-define-operator wrap-with-bold (beg end)
    (goto-char beg)
    (insert "*")
    (goto-char (1+ end))
    (insert "*"))

  (evil-define-key 'visual global-map
    (kbd "*") 'wrap-with-bold)

  (evil-define-operator wrap-with-verbatim (beg end)
    (goto-char beg)
    (insert "=")
    (goto-char (1+ end))
    (insert "="))

  (evil-define-key 'visual global-map
    (kbd "=") 'wrap-with-verbatim)

  (evil-define-operator wrap-with-code (beg end)
    (goto-char beg)
    (insert "~")
    (goto-char (1+ end))
    (insert "~"))

  (evil-define-key 'visual global-map
    (kbd "~") 'wrap-with-code)

  (evil-define-operator wrap-with-italic (beg end)
    (goto-char beg)
    (insert "/")
    (goto-char (1+ end))
    (insert "/"))

  (evil-define-key 'visual global-map
    (kbd "/") 'wrap-with-italic)

  ;; scroll-on-jump
  (scroll-on-jump-advice-add evil-undo)
  (scroll-on-jump-advice-add evil-redo)
  (scroll-on-jump-advice-add evil-jump-item)
  (scroll-on-jump-advice-add evil-jump-forward)
  (scroll-on-jump-advice-add evil-jump-backward)
  (scroll-on-jump-advice-add evil-ex-search-next)
  (scroll-on-jump-advice-add evil-ex-search-previous)
  (scroll-on-jump-advice-add evil-forward-paragraph)
  (scroll-on-jump-advice-add evil-backward-paragraph)
  (scroll-on-jump-advice-add evil-goto-mark)

  ;; Actions that themselves scroll.
  (scroll-on-jump-with-scroll-advice-add evil-goto-line)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-down)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-up)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-center)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-top)
  (scroll-on-jump-with-scroll-advice-add evil-scroll-line-to-bottom))


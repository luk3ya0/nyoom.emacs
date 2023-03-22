(add-to-list 'load-path "~/.doom.d/config/")

;;; UI ──────────────────────────────────────────────
(push '(width  . 91)                         default-frame-alist)
(push '(min-width  . 1)                      default-frame-alist)
(push '(height . 54)                         default-frame-alist)
(push '(min-height . 1)                      default-frame-alist)
(push '(internal-border-width . 14)          default-frame-alist)

(setq frame-title-format
      '(""
        (:eval
         (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                             (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;; (set-face-background 'default "mac:windowBackgroundColor")
;; (dolist (f (face-list)) (set-face-stipple f "alpha:30%"))
;; (setq face-remapping-alist (append face-remapping-alist '((default my/default-blurred))))
;; (defface my/default-blurred
;;    '((t :inherit 'default :stipple "alpha:30%"))
;;    "Like 'default but blurred."
;;    :group 'my)
;;
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
(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-serif-font doom-font
      doom-unicode-font (font-spec :family "PingFang SC" :size 15 :height 160)
      doom-variable-pitch-font (font-spec :family "PingFang SC" :size 15 :height 160))

(setq use-default-font-for-symbols nil)

(add-hook! 'after-setting-font-hook
  (set-fontset-font t 'latin (font-spec :family "Fira Code"))
  (set-fontset-font t 'symbol (font-spec :family "Fira Code Symbol"))
  (set-fontset-font t 'mathematical (font-spec :family "Fira Code Symbol"))
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji")))

(use-package! emojify
  :config
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c .") #'emojify-insert-emoji))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one-light)
(setq doom-theme 'doom-smoooooth-light)
;; (setq doom-theme 'doom-smoooooth)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Language mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local tab-width 2)))

(use-package! web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(defalias 'vue-mode 'web-mode)

(setq js-indent-level 2)

(add-hook 'html-mode-hook
          (lambda()
            (setq-local sgml-basic-offset 2)
            (setq-local indent-tabs-mode nil)))

(add-hook 'python-mode-hook (lambda ()
                              (setq python-indent 4)))

;;; Org ──────────────────────────────────────────────
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org")

(defface org-link-green
  '((t (:inherit org-link :foreground "medium sea green" :underline nil)))
  "A green link."
  :group `org-faces)

(defface org-progress-todo
  '((t (:inherit 'org-todo
        :foreground "azure2"
        :font-family "Fira Code"
        :height 150
        :avgwidth 160
        :spacing 100
        )))
  "Org mode todo face"
  :group 'org-face
  )

(defface org-progress-done
  '((t (:inherit 'org-todo
        :foreground "azure4"
        :font-family "Fira Code"
        :height 150
        :avgwidth 160
        :spacing 100)))
  "Org mode todo face"
  :group 'org-face
  )

(after! org
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :zoom 0.93) ; Calibrated based on the TeX font and org-buffer font.
  (org-link-set-parameters "file"
                           :face 'org-link-green)
  (set-face-attribute 'org-checkbox-statistics-todo nil
                      :inherit 'org-progress-todo
                      :width 'ultra-condensed
                      )
  (set-face-attribute 'org-checkbox-statistics-done nil
                      :inherit 'org-progress-done
                      :width 'ultra-condensed
                      )
  (setq org-archive-location (concat org-directory "roam/archive.org::")
        org-hide-leading-stars nil
        org-startup-indented nil
        org-edit-src-content-indentation 0
        org-image-actual-width nil
        org-startup-with-inline-images nil
        org-startup-with-latex-preview nil
        org-link-elisp-confirm-function nil
        org-link-frame-setup '((file . find-file))
        org-log-done t
        org-use-property-inheritance t
        org-confirm-babel-evaluate nil
        org-list-allow-alphabetical t
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
        org-fold-catch-invisible-edits 'smart
        org-latex-prefer-user-labels t
        org-startup-with-latex-preview nil
        org-format-latex-options (plist-put org-format-latex-options :scale 1.3)
        org-preview-latex-default-process 'dvisvgm
        org-preview-latex-process-alist'((dvisvgm :programs
                                          ("xelatex" "dvisvgm")
                                          :description "xdv > svg"
                                          :message "you need to install the programs: xelatex and dvisvgm."
                                          :use-xcolor t
                                          :image-input-type "xdv"
                                          :image-output-type "svg"
                                          :image-size-adjust (1.2 . 1.2)
                                          :latex-compiler
                                          ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                                          :image-converter
                                          ("dvisvgm %f -e -n -b min -c %S -o %O"))
                                         (imagemagick :programs
                                                      ("xelatex" "convert")
                                                      :description "pdf > png"
                                                      :message "you need to install the programs: xelatex and imagemagick."
                                                      :use-xcolor t
                                                      :image-input-type "pdf"
                                                      :image-output-type "png"
                                                      :image-size-adjust (1.0 . 1.0)
                                                      :latex-compiler
                                                      ("xelatex -interaction nonstopmode -output-directory %o %f")
                                                      :image-converter
                                                      ("convert -density %D -trim -antialias %f -quality 100 %O")))
        org-latex-compiler "xelatex"
        org-latex-packages-alist '(("" "amsthm")
                                   ("" "amsfonts")
                                   ("" "ctex")
                                   ("" "tikz")
                                   ("" "xcolor" t)
                                   ("cache=false" "minted" t))
        org-latex-pdf-process '("xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
                                "biber %b"
                                "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
                                "xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
                                "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
                                )
        )
  (setq org-emphasis-alist
        '(("*" (bold))
          ("/" italic)
          ("_" nil)
          ("=" (:background nil :foreground "pink4"))
          ("~" (:background nil :foreground "tan"))
          ;; ("+" (:strike-through t))
          )))

(dolist (hook '(org-mode-hook markdown-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local line-spacing 5)
                   (visual-line-mode 1)
                   (flyspell-mode -1)
                   (hl-line-mode -1))))

(use-package! valign
  :after org
  :diminish
  :hook
  (org-mode . valign-mode)
  :init
  (setq valign-fancy-bar t))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil))

(after! ox-hugo
  (setq org-hugo-use-code-for-kbd t))
;;; Behavior ────────────────────────────────────────
(global-subword-mode 1)      ; Iterate through CamelCase words

(setq-default major-mode 'org-mode)

;;; Editor > snippets & check ───────────────────────
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(org-mode)))

(use-package! doom-snippets
  :load-path "~/.doom.d/snippets"
  :after yasnippet)

;;; Editor > Motion ─────────────────────────────────
(setq undo-limit 80000000             ; Raise undo-limit to 80MB
      evil-want-fine-undo t           ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t             ; Nobody likes to loose work, I certainly don't
      )

(defun narrow-p ()
  "Return t if a buffer is narrowed"
  (not (equal (- (point-max) (point-min)) (buffer-size))))

(with-eval-after-load 'evil
  (defun normal-next-line()
    (interactive)
    (forward-line 1))

  (defun what-face ()
    "Get the actual face at point."
    (interactive)
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "%s" face) (message "No face"))))

  (defun what-org ()
    "Get the org-element-type at point."
    (interactive)
    (require 'org-element)
    (message "element type of %s, parent type of %s"
             (org-element-type (org-element-at-point))
             (org-element-type (org-element-property :parent (org-element-at-point)))))

  (defun normal-previous-line()
    (interactive)
    (forward-line -1))

  (defun emit-ocr ()
    (interactive)
    (insert (shell-command-to-string "/opt/homebrew/bin/ocr -l zh")))

  (defun emit-ocr-trim ()
    (interactive)
    (insert (string-trim (shell-command-to-string "/opt/homebrew/bin/ocr -l zh"))))

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
    (let (beg end sentinel pad current-char)
      (setq sentinel 32)
      (setq pad 2)
      (setq beg (point))
      (setq current-char (char-after))
      (while (not (equal current-char sentinel))
        (evil-forward-char 1 t)
        (if (equal (char-after) 40)
            (setq sentinel 41
                  pad 3))
        (setq current-char (char-after))
        (if (equal current-char 10)
            (setq current-char sentinel
                  pad 1)))
      (setq end (- (point) 1))
      (goto-char beg)
      (insert "*")
      (goto-char (+ end pad))
      (insert "*")
      ))

  (defun quickc ()
    (interactive)
    (let (beg end sentinel pad current-char)
      (setq sentinel 32)
      (setq pad 2)
      (setq beg (point))
      (setq current-char (char-after))
      (while (not (equal current-char sentinel))
        (evil-forward-char 1 t)
        (if (equal (char-after) 40)
            (setq sentinel 41
                  pad 3))
        (setq current-char (char-after))
        (if (equal current-char 10)
            (setq current-char sentinel
                  pad 1)))
      (setq end (- (point) 1))
      (goto-char beg)
      (insert "~")
      (goto-char (+ end pad))
      (insert "~")
      ))

  (defun quickv ()
    (interactive)
    (let (beg end sentinel pad current-char)
      (setq sentinel 32)
      (setq pad 2)
      (setq beg (point))
      (setq current-char (char-after))
      (while (not (equal current-char sentinel))
        (evil-forward-char 1 t)
        (if (equal (char-after) 40)
            (setq sentinel 41
                  pad 3))
        (setq current-char (char-after))
        (if (equal current-char 10)
            (setq current-char sentinel
                  pad 1)))
      (setq end (- (point) 1))
      (goto-char beg)
      (insert "=")
      (goto-char (+ end pad))
      (insert "=")
      ))

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
  (define-key evil-normal-state-map (kbd "s-,") 'quickv)
  (define-key evil-normal-state-map (kbd "s-;") 'quickc)
  (define-key evil-normal-state-map (kbd "s-[") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "s-]") 'next-buffer)
  (define-key evil-normal-state-map (kbd "s-f") 'cycle-format)
  (define-key evil-normal-state-map (kbd "RET") '+fold/toggle)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-o") 'toggle-narrow)
  (define-key evil-normal-state-map (kbd "s-p") 'what-face)
  (define-key evil-normal-state-map (kbd "s-o") 'what-org)

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

;;; Log ─────────────────────────────────────────────
(use-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))

(setq-default history-length 1000)

;; (use-package! hl-sentence
;;   :after org
;;   :diminish)

(defface fira-lock
  '((t (:font-family "Fira Code"
        :height 160
        :avgwidth 180
        :spacing 100
        )))
  "Org mode todo face"
  :group 'org-face)

(defvar log-font-lock-keywords
  `(
    ("\\[\\([0-9]\\{1,3\\}\\)%\\]"
     (0 (list 'face nil 'display (fira-code-progress-percent (match-string 1)))))
    ("\\[\\([0-9]+/[0-9]+\\)\\]"
     (0 (list 'face nil 'display (fira-code-progress-count (match-string 1)))))
    ;; ("\\(--\\)"
    ;;  (0 (list 'face 'fira-lock 'display (dash-to-hyphen (match-string 1)))))
    ;; ("\\(──\\)"
    ;;  (0 (list 'face 'fira-lock 'display (dash-to-hyphen (match-string 1)))))
    ))

;; (defun dash-to-hyphen (value)
;;   (format "%s" (make-string (length value) #x2500)))

(defun fira-code-progress-count (value)
  (concat (fira-code-progress-bar value) " " value))

(defun fira-code-progress-percent (value)
  (concat (fira-code-progress-bar (concat value "/100")) " " value "%"))

(defun fira-code-progress-bar (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))

    (let (comp uncomp bar)
      (setq comp (* (/ count total) 20))
      (setq uncomp (- 20 comp))
      (setq bar (format "%s%s"
                        (make-string (round comp) #xee04)
                        (make-string (round uncomp) #xee01)))
      (setq bar (substring bar 1 18))
      (if (= 0 comp)
          (setq bar (concat "\uee00" bar "\uee02")))
      (if (and
           (> comp 0)
           (< comp 20))
          (setq bar (concat "\uee03" bar "\uee02")))
      (if (= 20 comp)
          (setq bar (concat "\uee03" bar "\uee05")))
      bar
      )))

(add-hook 'org-mode-hook  (lambda ()
                            (push 'display font-lock-extra-managed-props)
                            (font-lock-add-keywords nil log-font-lock-keywords)
                            (font-lock-flush (point-min) (point-max))
                            ))

(add-hook 'emacs-lisp-mode-hook  (lambda ()
                                   (push 'display font-lock-extra-managed-props)
                                   (font-lock-add-keywords nil log-font-lock-keywords)
                                   (font-lock-flush (point-min) (point-max))
                                   ))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
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

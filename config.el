;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;;; Config Helpers ──────────────────────────────────────────────────────────────
;; `load!' for loading external *.el files relative to this one
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
;;; Private Info ────────────────────────────────────────────────────────────────

(setq user-full-name "Luke Yao"
      user-mail-address "oneTOinf@163.com")

;;; UI ──────────────────────────────────────────────────────────────────────────
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

;; (setq doom-theme 'doom-smoooooth-light)
(setq doom-theme 'doom-smoooooth)

(setq display-line-numbers-type nil)

(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-serif-font doom-font
      doom-unicode-font (font-spec :family "PingFang SC" :size 15 :height 150)
      doom-variable-pitch-font (font-spec :family "PingFang SC" :size 15 :height 150))

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

(setq fancy-splash-image
      (expand-file-name "misc/splash-images/avatar.png" doom-user-dir)
      +doom-dashboard-banner-padding '(0 . 0))

(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-user-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name,
and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))

;; remove useless dashboard info
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;;; Completion ──────────────────────────────────────────────────────────────────
;; (after! lsp-mode
;;   (setq lsp-enable-symbol-highlighting nil))

;; (after! lsp-ui
;;   (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
;;         lsp-ui-doc-enable nil))     ; redundant with K

(after! company
  (setq company-idle-delay 0.1
        company-selection-wrap-around t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers nil
        company-tooltip-limit 5
        company-tooltip-minimum-width 40)

  (set-company-backend!
    '(text-mode
      markdown-mode
      gfm-mode)
    '(:seperate
      company-files)))

(use-package doom-snippets
  :load-path "~/.config/doom/snippets"
  :after yasnippet)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))

(use-package! latex-preview-pane
  :init
  (setq pdf-latex-command "xelatex"))

(use-package! lsp-mode
  :config
  (setq
   lsp-latex-texlab-executable-argument-list
   '("-xelatex","-verbose","-file-line-error","-synctex=1","-interaction=nonstopmode","%f")))

;;; Action ──────────────────────────────────────────────────────────────────────
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

(map! :after vterm
      :map vterm-mode-map
      :ni "s-[" 'previous-buffer)

(map! :after vterm
      :map vterm-mode-map
      :ni "s-]" 'next-buffer)

;;; Language mode & Tree sitter ─────────────────────────────────────────────────
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
;;; Editor > Motion ─────────────────────────────────────────────────────────────
(setq undo-limit 80000000             ; Raise undo-limit to 80MB
      evil-want-fine-undo t           ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t             ; Nobody likes to loose work, I certainly don't
      )

(defun narrow-p ()
  "Return t if a buffer is narrowed"
  (not (equal (- (point-max) (point-min)) (buffer-size))))

(with-eval-after-load 'evil
  (setq sentence-end-base "[.?!…‽][]\\n\"'”’)}»›]*")
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
          (ov-map-put (current-sentence-beg) (current-sentence-end))
          )
      ))

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

(with-eval-after-load 'evil
  (defun normal-next-line()
    (interactive)
    (forward-line 1))

  (defun normal-previous-line()
    (interactive)
    (forward-line -1))

  (defun what-face ()
    "Get the actual face at point."
    (interactive)
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "%s" face) (message "No face"))))

  (defun emit-ocr ()
    (interactive)
    (insert (shell-command-to-string "/opt/homebrew/bin/ocr -l zh")))

  (defun emit-ocr-trim ()
    (interactive)
    (insert (string-trim (shell-command-to-string "/opt/homebrew/bin/ocr -l zh"))))


  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-insert-state-map (kbd "M-s-j") 'emit-ocr)
  (define-key evil-insert-state-map (kbd "M-s-k") 'emit-ocr-trim)

  (define-key evil-normal-state-map (kbd "RET") '+fold/toggle)
  (define-key evil-normal-state-map (kbd "s-p") 'what-face)

  (define-key evil-normal-state-map (kbd "s-[") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "s-]") 'next-buffer)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

;; scroll on jump with evil
(with-eval-after-load 'evil
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

;; scroll on image at cursor
(with-eval-after-load 'evil
  (defun scroll-up-org-mode()
    (interactive)
    (if (or (eq major-mode 'org-mode) (eq major-mode 'nov-mode))
        (iscroll-up)
      (evil-next-visual-line)))

  (defun scroll-down-org-mode()
    (interactive)
    (if (or (eq major-mode 'org-mode) (eq major-mode 'nov-mode))
        (iscroll-down)
      (evil-previous-visual-line)))

  ;; Use visual line motions even outside of visual-line-mode buffers
  (define-key evil-normal-state-map (kbd "C-n") 'scroll-up-org-mode)
  (define-key evil-normal-state-map (kbd "C-p") 'scroll-down-org-mode)
  (define-key evil-normal-state-map (kbd "j") 'scroll-up-org-mode)
  (define-key evil-normal-state-map (kbd "k") 'scroll-down-org-mode))

;;; Log ─────────────────────────────────────────────────────────────────────────
(use-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))

(setq-default history-length 1000)

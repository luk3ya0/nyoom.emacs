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
;; (use-package! web-mode
;;   :custom
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   (web-mode-code-indent-offset 2))

;; (defalias 'vue-mode 'web-mode)

;; (setq js-indent-level 2)

;; (add-hook 'html-mode-hook
;;           (lambda()
;;             (setq-local sgml-basic-offset 2)
;;             (setq-local indent-tabs-mode nil)))

;; (add-hook 'python-mode-hook (lambda ()
;;                               (setq python-indent 4)))
;;; Org Visual ──────────────────────────────────────────────────────────────────
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(after! org
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
          :spacing 100)))
    "Org mode todo face"
    :group 'org-face)

  (defface org-progress-done
    '((t (:inherit 'org-todo
          :foreground "azure4"
          :font-family "Fira Code"
          :height 150
          :avgwidth 160
          :spacing 100)))
    "Org mode todo face"
    :group 'org-face)
  (org-link-set-parameters "file"
                           :face 'org-link-green)
  (set-face-attribute 'org-checkbox-statistics-todo nil
                      :inherit 'org-progress-todo
                      :width 'ultra-condensed)
  (set-face-attribute 'org-checkbox-statistics-done nil
                      :inherit 'org-progress-done
                      :width 'ultra-condensed)
  (setq org-emphasis-alist
        '(("*" (bold))
          ("/" italic)
          ("_" nil)
          ("=" (:background nil :foreground "pink4"))
          ("~" (:background nil :foreground "tan"))))
  (setq org-archive-location (concat org-directory "roam/archive.org::")
        org-hide-leading-stars nil
        org-startup-indented nil
        org-edit-src-content-indentation 0
        org-display-inline-images t
        org-redisplay-inline-images t
        org-image-actual-width nil
        org-startup-with-inline-images nil
        org-startup-with-latex-preview nil
        org-link-elisp-confirm-function nil
        org-link-frame-setup '((file . find-file))
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
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
        org-fold-catch-invisible-edits 'smart))

(dolist (hook '(org-mode-hook markdown-mode-hook))
  (add-hook hook (lambda ()
                   (setq-local underline-minimum-offset 5)
                   (setq-local line-spacing 5)
                   (visual-line-mode 1)
                   (flyspell-mode -1)
                   (hl-line-mode -1))))

(use-package! iscroll
  :after org
  :diminish
  :hook
  (org-mode . iscroll-mode))

(use-package! valign
  :after org
  :diminish
  :hook
  (org-mode . valign-mode)
  :init
  (setq valign-fancy-bar t))

(use-package! org-appear
  :after org
  ;; :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil))

;;; Org Latex ───────────────────────────────────────────────────────────────────
(after! org
  (defun ob-latex-preamble (_)
    ;; (message (format "%s" params))
    (format "%s\n" "\\documentclass{standalone}"))
  (setq org-babel-latex-preamble #'ob-latex-preamble)
  (add-to-list 'org-preview-latex-process-alist '(xdvsvgm :progams
                                                  ("xelatex" "dvisvgm")
                                                  :discription "xdv > svg"
                                                  :message "you need install the programs: xelatex and dvisvgm."
                                                  :image-input-type "xdv"
                                                  :image-output-type "svg"
                                                  :image-size-adjust (1.8 . 1.5)
                                                  :latex-compiler ("gsed -i 's/\{article\}/\[tikz,dvisvgm\]\{article\}/g' %f"
                                                                   "cat %f > ~/file-bak.tex"
                                                                   "xelatex --shell-escape -interaction nonstopmode -no-pdf -output-directory %o %f")
                                                  :image-converter ("dvisvgm %f -n -b min -c %S -o %O"
                                                                    "gsed -i 's/#000/none/g' %O")))
  (setq org-preview-latex-default-process 'xdvsvgm)
  (setq org-latex-prefer-user-labels t
        org-startup-with-latex-preview nil
        org-latex-compiler "xelatex"
        org-latex-packages-alist '(("" "tikz" t)
                                   "\\usetikzlibrary{arrows.meta}"
                                   "\\usetikzlibrary{intersections}"
                                   "\\usetikzlibrary{angles,quotes}"

                                   ("" "fontspec" t)
                                   ("math-style=upright" "unicode-math" t)

                                   "\\setmainfont[Path=/Users/luke/Library/Fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont{Fira Math}"

                                   "\\setmathfont[slash-delimiter=frac]{Cambria Math}"

                                   "\\setmathfont[range=up,Path=/Users/luke/Library/Fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=sfup,Path=/Users/luke/Library/Fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=it,Path=/Users/luke/Library/Fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=bfup,Path=/Users/luke/Library/Fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=bfit,Path=/Users/luke/Library/Fonts/]{FiraCode-Medium.otf}"

                                   "\\setmathfont[range=\\sum]{latinmodern-math.otf}"

                                   "\\setmathfont[range={\"005B,\"005D}]{Fira Code}"
                                   "\\setmathfont[range={\"0021-\"003C}]{Fira Code}"
                                   )
        org-format-latex-options '(
                                   ;; :foreground "Black"
                                   ;; :background "Transparent"
                                   :scale 1.0
                                   :html-foreground "Black"
                                   ;; :html-background "Transparent" :html-scale 1.0
                                   :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        org-latex-pdf-process '("xelatex -8bit --shell-escape -interaction nonstopmode -output-directory=%o %f"
                                "biber %b"
                                ;; "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
                                ))

  
  (defun my/org-latex--get-tex-string ()
    "Return the content of the LaTeX fragment at point."
    (let ((datum (org-element-context)))
      (org-element-property :value datum)))

  (defun my/latex-fragment-superscript-p ()
    "Return `t' if '^' in current LaTeX fragment."
    (memq 94 (string-to-list (my/org-latex--get-tex-string))))

  (defun my/latex-tail-latin-p()
    (or
    (memq 103 (string-to-list (string-replace "igma" "" ;; Sigma
                                              (my/org-latex--get-tex-string))))
    (memq 106 (string-to-list (my/org-latex--get-tex-string)))
    (memq 112 (string-to-list (my/org-latex--get-tex-string)))
    (memq 113 (string-to-list (string-replace "equiv" "" ;; equiv
                                              (my/org-latex--get-tex-string))))
    (memq 121 (string-to-list (string-replace "infty" "" ;; infty
                                              (my/org-latex--get-tex-string))))
     ))

  (defun my/latex-fragment-subscript-p ()
    "Return `t' if '_' in current LaTeX fragment."
     (memq 95 (string-to-list (my/org-latex--get-tex-string))))

  (defun my/latex-fragment-script-p ()
    "Return `t' if both '_' &  '^' in current LaTeX fragment."
     (and (memq 94 (string-to-list (my/org-latex--get-tex-string)))
          (memq 95 (string-to-list (my/org-latex--get-tex-string)))))

  (defun my/latex-fragment-frac-p ()
    "Return `t' if contain frac in current LaTeX fragment."
     (string-match "frac" (my/org-latex--get-tex-string)))

  (defun my/latex-fragment-cfrac-and-subscript-p ()
    "Return `t' if contain frac in current LaTeX fragment."
    (and
     (my/latex-fragment-subscript-p)
     (string-match "cfrac" (my/org-latex--get-tex-string))
     ))

  (defun my/latex-fragment-bracket-p ()
    "Return `t' if '(' in current LaTeX fragment."
    (memq 40 (string-to-list (my/org-latex--get-tex-string))))

  (defun org--make-preview-overlay (beg end image &optional imagetype)
    "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
    (setq my/position 100)
    (cond ((my/latex-fragment-cfrac-and-subscript-p)
           (setq my/position 59))
          ((my/latex-fragment-frac-p)
           (setq my/position 67))
          ((my/latex-fragment-bracket-p)
           (setq my/position 87))
          ((and (my/latex-fragment-subscript-p) (my/latex-tail-latin-p))
           (setq my/position 69))
          ((or (my/latex-fragment-script-p) (my/latex-tail-latin-p))
           (setq my/position 72))
          ((my/latex-fragment-superscript-p)
           (setq my/position 98))
          ((my/latex-fragment-subscript-p)
           (setq my/position 78)))
    (let ((ov (make-overlay beg end))
          (imagetype (or (intern imagetype) 'png)))
      (overlay-put ov 'org-overlay-type 'org-latex-overlay)
      (overlay-put ov 'evaporate t)
      (overlay-put ov
                   'modification-hooks
                   (list (lambda (o _flag _beg _end &optional _l)
                           (delete-overlay o))))
      (overlay-put ov
                   'display
                   (list 'image :type imagetype :file image :ascent my/position))))

  (require 'ov)
  ;; * Fragment justification
  (defun scimax-org-latex-fragment-justify (justification)
    "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
    (interactive
     (list (intern-soft
            (completing-read "Justification (left): " '(left center right)
                             nil t nil nil 'left))))
    (let* ((ov (ov-at))
           (beg (ov-beg ov))
           (end (ov-end ov))
           (shift (- beg (line-beginning-position)))
           (img (overlay-get ov 'display))
           (img (and (and img (consp img) (eq (car img) 'image)
                          (image-type-available-p (plist-get (cdr img) :type)))
                     img))
           space-left offset)
      (when (and img
                 ;; This means the equation is at the start of the line
                 (= beg (line-beginning-position))
                 (or
                  (string= "" (s-trim (buffer-substring end (line-end-position))))
                  (eq 'latex-environment (car (org-element-context)))))
        (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
              offset (floor (cond
                             ((eq justification 'center)
                              (- (/ space-left 2) shift))
                             ((eq justification 'right)
                              (- space-left shift))
                             (t
                              0))))
        (when (>= offset 0)
          (overlay-put ov 'before-string (make-string offset ?\ ))))))

  (defun scimax-org-latex-fragment-justify-advice (_ _ _ _)
    "After advice function to justify fragments."
    (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'center)))


  (advice-add 'org--make-preview-overlay :after 'scimax-org-latex-fragment-justify-advice)
  )
;;; Editor > Motion ─────────────────────────────────────────────────────────────
(setq undo-limit 80000000             ; Raise undo-limit to 80MB
      evil-want-fine-undo t           ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t             ; Nobody likes to loose work, I certainly don't
      )

(defun narrow-p ()
  "Return t if a buffer is narrowed"
  (not (equal (- (point-max) (point-min)) (buffer-size))))

(with-eval-after-load 'evil
  (require 'ov)

  (setq sentence-end-base "[.?!…‽][]\\n\"'”’)}»›]*")

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

  (defun what-org ()
    "Get the org-element-type at point."
    (interactive)
    (require 'org-element)
    (message "element type of %s, parent type of %s"
             (org-element-type (org-element-at-point))
             (org-element-property :language (org-element-at-point))))

  (defun emit-ocr ()
    (interactive)
    (insert (shell-command-to-string "/opt/homebrew/bin/ocr -l zh")))

  (defun emit-ocr-trim ()
    (interactive)
    (insert (string-trim (shell-command-to-string "/opt/homebrew/bin/ocr -l zh"))))

  (defun cycle-format ()
    (interactive)
    (require 'org-element)
    (setq lang (org-element-property :language (org-element-at-point)))
    (message "src block lang %s" lang)
    (if (string-equal lang "java")
        (progn
          (org-edit-special)
          (kill-region (point-min) (point-max))
          ;; (shell-command "pbpaste | google-java-format --aosp - | pbcopy")
          (insert (shell-command-to-string "pbpaste | google-java-format --aosp -"))
          ))

    (if (string-equal lang "go")
        (progn
          (org-edit-special)
          (kill-region (point-min) (point-max))
          ;; (shell-command "pbpaste | gofmt | gsed 's/\t/    /g' | pbcopy")
          (insert (shell-command-to-string "pbpaste | gofmt | gsed 's/\t/    /g'"))
          ))

    ;; indent the region anyway
    (if (not (string-equal lang "go"))
        (indent-region (point-min) (point-max)))
    (org-edit-src-exit)
    (save-buffer))

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

  (defun scroll-up-org-mode()
    (interactive)
    (if (eq major-mode 'org-mode)
        (iscroll-up)
      (evil-next-visual-line)))

  (defun scroll-down-org-mode()
    (interactive)
    (if (eq major-mode 'org-mode)
        (iscroll-down)
      (evil-previous-visual-line)))

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
  (define-key evil-normal-state-map (kbd "M-p") 'underline-backward)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-insert-state-map (kbd "M-s-j") 'emit-ocr)
  (define-key evil-insert-state-map (kbd "M-s-k") 'emit-ocr-trim)

  (define-key evil-normal-state-map (kbd "s-b") 'quickb)
  (define-key evil-normal-state-map (kbd "s-,") 'quickv)
  (define-key evil-normal-state-map (kbd "s-;") 'quickc)
  (define-key evil-normal-state-map (kbd "s-[") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "s-]") 'next-buffer)
  (define-key evil-normal-state-map (kbd "s-f") 'cycle-format)
  (define-key evil-normal-state-map (kbd "RET") '+fold/toggle)
  (define-key evil-normal-state-map (kbd "C-n") 'scroll-up-org-mode)
  (define-key evil-normal-state-map (kbd "C-p") 'scroll-down-org-mode)
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
;;; Log ─────────────────────────────────────────────────────────────────────────
(use-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))

(setq-default history-length 1000)

;;; TODO ────────────────────────────────────────────────────────────────────────
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
           (setq bar (concat "\uee00" bar "\uee02"))
         )
       (if (and
            (> comp 0)
            (< comp 20)
            )
           (setq bar (concat "\uee03" bar "\uee02"))
         )
       (if (= 20 comp)
           (setq bar (concat "\uee03" bar "\uee05"))
         )
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
  ;; (let (org-log-done org-log-states)   ; turn off logging
  (let (org-log-done)   ; turn off logging
    (message (format "%s" n-done))
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

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


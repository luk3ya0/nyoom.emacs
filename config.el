;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; UI
(push '(width  . 91)                         default-frame-alist)
(push '(min-width  . 1)                      default-frame-alist)
(push '(height . 54)                         default-frame-alist)
(push '(min-height . 1)                      default-frame-alist)
(push '(left-fringe    . 29)                 default-frame-alist)
(push '(right-fringe   . 29)                 default-frame-alist)
(push '(internal-border-width . 14)          default-frame-alist)
;; (push `(alpha . ,'(95 . 95))                 default-frame-alist)

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
(setq doom-unicode-font (font-spec :family "PingFang SC" :size 15))
(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'semi-light))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-smoooooth-light)
(setq doom-theme 'doom-smoooooth)

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
    '((t :family "Fira Code" :size 15 :bold nil :foreground "#59B0CF"))
    "Face used by org-ol-tree to display the root node."
    :group 'org-ol-tree-faces)

  (defface org-ol-tree-section-title-face
    '((t :inherit font-lock-doc-face :family "Fira Code" :size 15))
    "Face used by org-ol-tree to display section titles."
    :group 'org-ol-tree-faces)

  (defface org-ol-tree-section-id-face
    '((t :inherit treemacs-file-face :family "Fira Code" :size 15))
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

;;; Org > Exporting
(after! ox-html
  (require 'f)
        (setq org-export-headline-levels 5) ; I like nesting

        (require 'ox-extra)
        (ox-extras-activate '(ignore-headlines))

        (setq org-export-creator-string
        (format "Emacs %s (Org mode %s–%s)" emacs-version (org-release) (org-git-version)))

        (define-minor-mode org-fancy-html-export-mode
        "Toggle my fabulous org export tweaks. While this mode itself does a little bit,
        the vast majority of the change in behaviour comes from switch statements in:
        - `org-html-template-fancier'
        - `org-html--build-meta-info-extended'
        - `org-html-src-block-collapsable'
        - `org-html-block-collapsable'
        - `org-html-table-wrapped'
        - `org-html--format-toc-headline-colapseable'
        - `org-html--toc-text-stripped-leaves'
        - `org-export-html-headline-anchor'"
        :global t
        :init-value t
        (if org-fancy-html-export-mode
        (setq org-html-style-default org-html-style-fancy
                org-html-meta-tags #'org-html-meta-tags-fancy
                org-html-checkbox-type 'html-span)
        (setq org-html-style-default org-html-style-plain
                org-html-meta-tags #'org-html-meta-tags-default
                org-html-checkbox-type 'html)))

        (defadvice! org-html-template-fancier (orig-fn contents info)
        "Return complete document string after HTML conversion.
        CONTENTS is the transcoded contents string.  INFO is a plist
        holding export options. Adds a few extra things to the body
        compared to the default implementation."
        :around #'org-html-template
        (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
        (funcall orig-fn contents info)
        (concat
        (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
        (let* ((xml-declaration (plist-get info :html-xml-declaration))
                (decl (or (and (stringp xml-declaration) xml-declaration)
                                (cdr (assoc (plist-get info :html-extension)
                                        xml-declaration))
                                (cdr (assoc "html" xml-declaration))
                                "")))
                (when (not (or (not decl) (string= "" decl)))
                (format "%s\n"
                        (format decl
                                (or (and org-html-coding-system
                                        (fboundp 'coding-system-get)
                                        (coding-system-get org-html-coding-system 'mime-charset))
                                "iso-8859-1"))))))
        (org-html-doctype info)
        "\n"
        (concat "<html"
                (cond ((org-html-xhtml-p info)
                        (format
                        " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
                        (plist-get info :language) (plist-get info :language)))
                        ((org-html-html5-p info)
                        (format " lang=\"%s\"" (plist-get info :language))))
                ">\n")
        "<head>\n"
        (org-html--build-meta-info info)
        (org-html--build-head info)
        (org-html--build-mathjax-config info)
        "</head>\n"
        "<body>\n<input type='checkbox' id='theme-switch'><div id='page'><label id='switch-label' for='theme-switch'></label>"
        (let ((link-up (org-trim (plist-get info :html-link-up)))
                (link-home (org-trim (plist-get info :html-link-home))))
        (unless (and (string= link-up "") (string= link-home ""))
                (format (plist-get info :html-home/up-format)
                        (or link-up link-home)
                        (or link-home link-up))))
        ;; Preamble.
        (org-html--build-pre/postamble 'preamble info)
        ;; Document contents.
        (let ((div (assq 'content (plist-get info :html-divs))))
        (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
        ;; Document title.
        (when (plist-get info :with-title)
        (let ((title (and (plist-get info :with-title)
                                (plist-get info :title)))
                (subtitle (plist-get info :subtitle))
                (html5-fancy (org-html--html5-fancy-p info)))
                (when title
                (format
                (if html5-fancy
                        "<header class=\"page-header\">%s\n<h1 class=\"title\">%s</h1>\n%s</header>"
                "<h1 class=\"title\">%s%s</h1>\n")
                (if (or (plist-get info :with-date)
                        (plist-get info :with-author))
                        (concat "<div class=\"page-meta\">"
                                (when (plist-get info :with-date)
                                (org-export-data (plist-get info :date) info))
                                (when (and (plist-get info :with-date) (plist-get info :with-author)) ", ")
                                (when (plist-get info :with-author)
                                (org-export-data (plist-get info :author) info))
                                "</div>\n")
                "")
                (org-export-data title info)
                (if subtitle
                        (format
                        (if html5-fancy
                        "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
                        (concat "\n" (org-html-close-tag "br" nil info) "\n"
                                "<span class=\"subtitle\">%s</span>\n"))
                        (org-export-data subtitle info))
                "")))))
        contents
        (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
        ;; Postamble.
        (org-html--build-pre/postamble 'postamble info)
        ;; Possibly use the Klipse library live code blocks.
        (when (plist-get info :html-klipsify-src)
        (concat "<script>" (plist-get info :html-klipse-selection-script)
                "</script><script src=\""
                org-html-klipse-js
                "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
                org-html-klipse-css "\"/>"))
        ;; Closing document.
        "</div>\n</body>\n</html>")))

        (setq org-html-style-plain org-html-style-default
        org-html-htmlize-output-type 'css
        org-html-doctype "html5"
        org-html-html5-fancy t)

        (defun org-html-reload-fancy-style ()
        (interactive)
        (setq org-html-style-fancy
                (concat (f-read-text (expand-file-name "misc/org-export-header.html" doom-private-dir))
                        "<script>\n"
                        (f-read-text (expand-file-name "misc/org-css/main.js" doom-private-dir))
                        "</script>\n<style>\n"
                        (f-read-text (expand-file-name "misc/org-css/main.css" doom-private-dir))
                        "</style>"))
        (when org-fancy-html-export-mode
        (setq org-html-style-default org-html-style-fancy)))
        (org-html-reload-fancy-style)

        (defvar org-html-export-collapsed nil)
        (eval '(cl-pushnew '(:collapsed "COLLAPSED" "collapsed" org-html-export-collapsed t)
                        (org-export-backend-options (org-export-get-backend 'html))))
        (add-to-list 'org-default-properties "EXPORT_COLLAPSED")

        (defadvice! org-html-src-block-collapsable (orig-fn src-block contents info)
        "Wrap the usual <pre> block in a <details>"
        :around #'org-html-src-block
        (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
        (funcall orig-fn src-block contents info)
        (let* ((properties (cadr src-block))
                (lang (mode-name-to-lang-name
                        (plist-get properties :language)))
                (name (plist-get properties :name))
                (ref (org-export-get-reference src-block info))
                (collapsed-p (member (or (org-export-read-attribute :attr_html src-block :collapsed)
                                        (plist-get info :collapsed))
                                        '("y" "yes" "t" t "true" "all"))))
        (format
        "<details id='%s' class='code'%s><summary%s>%s</summary>
        <div class='gutter'>
        <a href='#%s'>#</a>
        <button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
        </div>
        %s
        </details>"
        ref
        (if collapsed-p "" " open")
        (if name " class='named'" "")
        (concat
                (when name (concat "<span class=\"name\">" name "</span>"))
                "<span class=\"lang\">" lang "</span>")
        ref
        (if name
                (replace-regexp-in-string (format "<pre\\( class=\"[^\"]+\"\\)? id=\"%s\">" ref) "<pre\\1>"
                                        (funcall orig-fn src-block contents info))
                (funcall orig-fn src-block contents info))))))

        (defun mode-name-to-lang-name (mode)
        (or (cadr (assoc mode
                        '(("asymptote" "Asymptote")
                        ("awk" "Awk")
                        ("C" "C")
                        ("clojure" "Clojure")
                        ("css" "CSS")
                        ("D" "D")
                        ("ditaa" "ditaa")
                        ("dot" "Graphviz")
                        ("calc" "Emacs Calc")
                        ("emacs-lisp" "Emacs Lisp")
                        ("fortran" "Fortran")
                        ("gnuplot" "gnuplot")
                        ("haskell" "Haskell")
                        ("hledger" "hledger")
                        ("java" "Java")
                        ("js" "Javascript")
                        ("latex" "LaTeX")
                        ("ledger" "Ledger")
                        ("lisp" "Lisp")
                        ("lilypond" "Lilypond")
                        ("lua" "Lua")
                        ("matlab" "MATLAB")
                        ("mscgen" "Mscgen")
                        ("ocaml" "Objective Caml")
                        ("octave" "Octave")
                        ("org" "Org mode")
                        ("oz" "OZ")
                        ("plantuml" "Plantuml")
                        ("processing" "Processing.js")
                        ("python" "Python")
                        ("R" "R")
                        ("ruby" "Ruby")
                        ("sass" "Sass")
                        ("scheme" "Scheme")
                        ("screen" "Gnu Screen")
                        ("sed" "Sed")
                        ("sh" "shell")
                        ("sql" "SQL")
                        ("sqlite" "SQLite")
                        ("forth" "Forth")
                        ("io" "IO")
                        ("J" "J")
                        ("makefile" "Makefile")
                        ("maxima" "Maxima")
                        ("perl" "Perl")
                        ("picolisp" "Pico Lisp")
                        ("scala" "Scala")
                        ("shell" "Shell Script")
                        ("ebnf2ps" "ebfn2ps")
                        ("cpp" "C++")
                        ("abc" "ABC")
                        ("coq" "Coq")
                        ("groovy" "Groovy")
                        ("bash" "bash")
                        ("csh" "csh")
                        ("ash" "ash")
                        ("dash" "dash")
                        ("ksh" "ksh")
                        ("mksh" "mksh")
                        ("posh" "posh")
                        ("ada" "Ada")
                        ("asm" "Assembler")
                        ("caml" "Caml")
                        ("delphi" "Delphi")
                        ("html" "HTML")
                        ("idl" "IDL")
                        ("mercury" "Mercury")
                        ("metapost" "MetaPost")
                        ("modula-2" "Modula-2")
                        ("pascal" "Pascal")
                        ("ps" "PostScript")
                        ("prolog" "Prolog")
                        ("simula" "Simula")
                        ("tcl" "tcl")
                        ("tex" "LaTeX")
                        ("plain-tex" "TeX")
                        ("verilog" "Verilog")
                        ("vhdl" "VHDL")
                        ("xml" "XML")
                        ("nxml" "XML")
                        ("conf" "Configuration File"))))
        mode))

        (defun org-html-block-collapsable (orig-fn block contents info)
        "Wrap the usual block in a <details>"
        (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
        (funcall orig-fn block contents info)
        (let ((ref (org-export-get-reference block info))
                (type (pcase (car block)
                        ('property-drawer "Properties")))
                (collapsed-default (pcase (car block)
                                ('property-drawer t)
                                (_ nil)))
                (collapsed-value (org-export-read-attribute :attr_html block :collapsed))
                (collapsed-p (or (member (org-export-read-attribute :attr_html block :collapsed)
                                        '("y" "yes" "t" t "true"))
                                (member (plist-get info :collapsed) '("all")))))
        (format
        "<details id='%s' class='code'%s>
        <summary%s>%s</summary>
        <div class='gutter'>\
        <a href='#%s'>#</a>
        <button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
        </div>
        %s\n
        </details>"
        ref
        (if (or collapsed-p collapsed-default) "" " open")
        (if type " class='named'" "")
        (if type (format "<span class='type'>%s</span>" type) "")
        ref
        (funcall orig-fn block contents info)))))

        (advice-add 'org-html-example-block   :around #'org-html-block-collapsable)
        (advice-add 'org-html-fixed-width     :around #'org-html-block-collapsable)
        (advice-add 'org-html-property-drawer :around #'org-html-block-collapsable)

        (defadvice! org-html-table-wrapped (orig-fn table contents info)
        "Wrap the usual <table> in a <div>"
        :around #'org-html-table
        (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
        (funcall orig-fn table contents info)
        (let* ((name (plist-get (cadr table) :name))
                (ref (org-export-get-reference table info)))
        (format "<div id='%s' class='table'>
        <div class='gutter'><a href='#%s'>#</a></div>
        <div class='tabular'>
        %s
        </div>\
        </div>"
                ref ref
                (if name
                        (replace-regexp-in-string (format "<table id=\"%s\"" ref) "<table"
                                                (funcall orig-fn table contents info))
                        (funcall orig-fn table contents info))))))
  )
;;; Behavior
(global-subword-mode 1)      ; Iterate through CamelCase words

(setq-default major-mode 'org-mode)

;;; Windows
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;;; Editor > snippets
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(org-mode)))

(use-package doom-snippets
  :load-path "~/.doom.d/editors/snippets"
  :after yasnippet)


;;; Editor > Motion
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
    (interactive)
    (message "char-after: %s" (char-after)))
    ;; (message "thing at point: %s" (thing-at-point 'symbol)))
    ;; (let ((face (or (get-char-property (point) 'read-face-name)
    ;;                 (get-char-property (point) 'face))))
    ;;   (if face (message "Face: %s" face) (message "No face at %d" (point)))))

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

;;; Log
(use-package! command-log-mode
   :commands global-command-log-mode
   :config
   (setq command-log-mode-auto-show t
         command-log-mode-open-log-turns-on-mode nil
         command-log-mode-is-global t
         command-log-mode-window-size 50))
;;; Company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying

(setq-default history-length 1000)

;;; Keycast in modeline

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))

;;; Calibre
;; (use-package! calibredb
;;   :commands calibredb
;;   :config
;;   (setq calibredb-root-dir "~/Desktop/TEC/Other/Ebooks"
;;         calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))
;; (use-package! nov
;;   :mode ("\\.epub\\'" . nov-mode)
;;   :config
;;   (map! :map nov-mode-map
;;         :n "RET" #'nov-scroll-up)

;;   (defun doom-modeline-segment--nov-info ()
;;     (concat
;;      " "
;;      (propertize
;;       (cdr (assoc 'creator nov-metadata))
;;       'face 'doom-modeline-project-parent-dir)
;;      " "
;;      (cdr (assoc 'title nov-metadata))
;;      " "
;;      (propertize
;;       (format "%d/%d"
;;               (1+ nov-documents-index)
;;               (length nov-documents))
;;       'face 'doom-modeline-info)))

;;   (advice-add 'nov-render-title :override #'ignore)

;;   (defun +nov-mode-setup ()
;;     (face-remap-add-relative 'variable-pitch
;;                              :family "Merriweather"
;;                              :height 1.4
;;                              :width 'semi-expanded)
;;     (face-remap-add-relative 'default :height 1.3)
;;     (setq-local line-spacing 0.2
;;                 next-screen-context-lines 4
;;                 shr-use-colors nil)
;;     (require 'visual-fill-column nil t)
;;     (setq-local visual-fill-column-center-text t
;;                 visual-fill-column-width 81
;;                 nov-text-width 80)
;;     (visual-fill-column-mode 1)
;;     (hl-line-mode -1)

;;     (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)

;;     (setq-local mode-line-format
;;                 `((:eval
;;                    (doom-modeline-segment--workspace-name))
;;                   (:eval
;;                    (doom-modeline-segment--window-number))
;;                   (:eval
;;                    (doom-modeline-segment--nov-info))
;;                   ,(propertize
;;                     " %P "
;;                     'face 'doom-modeline-buffer-minor-mode)
;;                   ,(propertize
;;                     " "
;;                     'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
;;                     'display `((space
;;                                 :align-to
;;                                 (- (+ right right-fringe right-margin)
;;                                    ,(* (let ((width (doom-modeline--font-width)))
;;                                          (or (and (= width 1) 1)
;;                                              (/ width (frame-char-width) 1.0)))
;;                                        (string-width
;;                                         (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
;;                   (:eval (doom-modeline-segment--major-mode)))))

;;   (add-hook 'nov-mode-hook #'+nov-mode-setup))


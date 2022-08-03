;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
(setq doom-unicode-font (font-spec :family "PingFang SC" :size 24))
(setq doom-font (font-spec :family "Fira Code" :size 24 :weight 'semi-light))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local line-spacing 0.3)))

(setq org-directory "~/Documents/Org")

(after! org
  (setq org-archive-location (concat org-directory "roam/archive.org::")
        org-display-inline-images t
        org-redisplay-inline-images t
        org-latex-default-class "ctexart"
        org-latex-compiler "xelatex"
        org-startup-with-inline-images "inlineimages"
        org-startup-with-latex-preview "latexpreview"
        org-link-elisp-confirm-function nil
        org-link-frame-setup '((file . find-file))
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 0.85)
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
        org-catch-invisible-edits 'smart))

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

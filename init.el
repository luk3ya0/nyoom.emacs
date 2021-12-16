;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Package
(require 'init-package)

(require 'init-evil)

(require 'init-hydra)

(require 'init-basic)

(require 'init-ui)

(require 'init-dashboard)

(require 'init-treemacs)

(require 'init-ivy)

(require 'init-company)

(require 'init-org)

(require 'init-markdown)

;; (set-fontset-font t '(#xe903 . #xfffd) "all-the-icons")
;; (set-fontset-font t '(#x00a2 . #xf17b) "file-icons")
;; (set-fontset-font t '(#x2665 . #xf27c) "github-octicons")
;; (set-fontset-font t '(#x2122 . #xf4a4) "FontAwesome")
;; (set-fontset-font t '(#xf000 . #xf0eb) "Weather Icons")

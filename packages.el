;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
;;; Org Packages ────────────────────────────────────────────────────────────────
(package! valign
  :recipe (:host github :repo "casouri/valign"
           :files ("*.el")))

(package! texfrag)

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))

(package! org-fragtog
  :recipe (:host github :repo "io12/org-fragtog"))

(package! ov
  :recipe (:host github :repo "emacsorphanage/ov"))

(disable-packages! htmlsize org-superstar)

(package! hl-sentence
  :recipe (:host github :repo "luk3ya0/hl-sentence"
          :files ("*.el" "lisp/*.el")))

(package! iscroll
  :recipe (:host github :repo "luk3ya0/iscroll"
          :files ("*.el" "lisp/*.el")))

(package! svg-lib
  :recipe (:host github :repo "luk3ya0/svg-lib"
          :files ("*.el" "lisp/*.el")))

(package! org
  :recipe (:host github
           ;; REVIEW: I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "luk3ya0/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK: Org has a post-install step that generates org-version.el
           ;;   and org-loaddefs.el, but Straight doesn't invoke this step, and
           ;;   the former doesn't work if the Org repo is a shallow clone.
           ;;   Rather than impose the network burden of a full clone (and other
           ;;   redundant work in Org's makefile), I'd rather fake these files
           ;;   instead. Besides, Straight already produces a org-autoloads.el,
           ;;   so org-loaddefs.el isn't needed.
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
             (with-temp-file "org-version.el"
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                        (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                            (match-string-no-properties 1)
                          "Unknown"))))
                 (insert (format "(defun org-release () %S)\n" version)
                         (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                                 version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                         "(provide 'org-version)\n"))))))
;;; Motion & Log ────────────────────────────────────────────────────────────────
(package! scroll-on-jump)

(package! command-log-mode)

;;; Magit Packages ──────────────────────────────────────────────────────────────
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))

;;; Language mode ───────────────────────────────────────────────────────────────
;;; These packages are only to support syntax highlight in org mode src block
(package! go-mode)

(package! dart-mode)

(package! swift-mode)

(package! kotlin-mode)

(package! lua-mode)

(package! json-mode)

(package! rustic)

(package! rjsx-mode)

(package! typescript-mode)


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
          :size 8
          :height 80
          :avgwidth 80
          :spacing 100)))
    "Org mode todo face"
    :group 'org-face)

  (defface org-progress-done
    '((t (:inherit 'org-todo
          :foreground "azure4"
          :font-family "Fira Code"
          :size 8
          :height 80
          :avgwidth 80
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
          ("=" (:foreground "pink4"))
          ("~" (:foreground "tan"))))
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
  (org-mode . iscroll-mode)
  (nov-mode . iscroll-mode))

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

;; (use-package! org-fragtog
;;   :after org
;;   :hook (org-mode . org-fragtog-mode))

;;; Org Latex ───────────────────────────────────────────────────────────────────
(after! org
  (add-to-list 'org-preview-latex-process-alist
               '(xdvsvgm :progams
                 ("xelatex" "dvisvgm")
                 :discription "xdv > svg"
                 :message "you need install the programs: xelatex and dvisvgm."
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (0.85 . 0.9)
                 :latex-compiler (
                                  "sed -i 's/\{article\}/\[dvisvgm\]\{article\}/g' %f"
                                  ;; "gsed -i 's/\{article\}/\[tikz,dvisvgm\]\{article\}/g' %f"
                                  "cat %f > ~/file-bak.tex"
                                  "xelatex --shell-escape -interaction nonstopmode -no-pdf -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"
                                   "sed -i 's/#000/none/g' %O")))
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
                                   ("" "firamath-otf" t)

                                   "\\setmainfont{Fira Code}"
                                   "\\setmathfont{Fira Math}"


                                   "\\setmathfont[range=up,Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=sfup,Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=it,Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=bfup,Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range=bfit,Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"

                                   "\\setmathfont[range={\"2261,\"2262},Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"
                                   "\\setmathfont[range={\"0021-\"003E},Path=/home/luke/\.fonts/]{FiraCode-Medium.otf}"
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
  (defun ob-latex-preamble (params)
    ;; (message (format "%s" params))

    (mapconcat 'identity '("\\documentclass[dvisvgm]{article}"
              "\\usepackage[usenames]{color}"
              "\\usepackage{graphicx}"
              "\\usepackage[normalem]{ulem}"
              "\\usepackage{amsmath}"
              "\\usepackage{amssymb}"
              "\\usepackage{nicematrix}"

              "\\usepackage{tabularray}"
              "\\usepackage{rotating}"
              "\\usepackage{multirow}"
              "\\usepackage{mathtools}"
              "\\def\\block(#1,#2)#3{\\multicolumn{#2}{c}{\\multirow{#1}{*}{$ #3 $}}}"

              "\\usepackage{tikz}"
              "\\usetikzlibrary{arrows.meta}"
              "\\usetikzlibrary{intersections}"
              "\\usetikzlibrary{angles,quotes}"
              "\\usepackage{fontspec}"
              "\\usepackage[math-style=upright]{unicode-math}"

              "\\usepackage[sfdefault,lining]{FiraSans} %% option 'sfdefault' activates Fira Sans as the default text font"
              "\\usepackage[fakebold]{firamath-otf}"
              "\\renewcommand*\\oldstylenums[1]{{\\firaoldstyle #1}}"

              "\\pagestyle{empty}"
              "\\setlength{\\textwidth}{\\paperwidth}"
              "\\addtolength{\\textwidth}{-3cm}"
              "\\setlength{\\oddsidemargin}{1.5cm}"
              "\\addtolength{\\oddsidemargin}{-2.54cm}"
              "\\setlength{\\evensidemargin}{\\oddsidemargin}"
              "\\setlength{\\textheight}{\\paperheight}"
              "\\addtolength{\\textheight}{-\\headheight}"
              "\\addtolength{\\textheight}{-\\headsep}"
              "\\addtolength{\\textheight}{-\\footskip}"
              "\\addtolength{\\textheight}{-3cm}"
              "\\setlength{\\topmargin}{1.5cm}"
              "\\addtolength{\\topmargin}{-2.54cm}"
              ) "\n" ))

  (setq org-babel-latex-preamble #'ob-latex-preamble)

  (defun my/org-latex--get-tex-string ()
    "Return the content of the LaTeX fragment at point."
    (let ((datum (org-element-context)))
      (org-element-property :value datum)))

  (defun my/latex-fragment-superscript-p ()
    "Return `t' if '^' in current LaTeX fragment."
    (memq 94 (string-to-list (my/org-latex--get-tex-string))))

  (defun my/latex-tail-latin-p()
    (let (tex-string)
      (setq tex-string (my/org-latex--get-tex-string))
      (dolist (ele '("alpha" "Alpha" "gamma" "Gamma" "pi" "Pi" "epsilon" "Epsilon"
                     "sigma" "Sigma" "upsilon" "Upsilon" "kappa" "Kappa" "psi" "Psi"
                     "omega" "Omega" ;; greek symbol
                     "rightarrow" "Rightarrow" "leftrightarrow" "Leftrightarrow"
                     "uparrow" "Uparrow" "updownarrow" "Updownarrow"
                     "mapsto" "longmapsto" "leftharpoonup" "rightharpoonup"
                     "leftharpoondown" "rightharpoondown" ;; arrow
                     "infty" "partial" "emptyset" "varnothing" "complement"
                     "neg" "square" "blacksquare" "triangle" ;; miscellaneous symbols
                     "cap" "cup" "bigcap" "bigcup" "neq" "leq" "geq" "perp"
                     "simeq" "approx" "wedge" "oplus" "equiv" "cong" ;; binary operation/relation symbol
                     "subseteq" "supseteq" "sqrt" "angle" "measuredangle" "pm"
                     "sphericalangle" "varangle"
                     "^{g" "^{j" "^{p" "^{q" "^{y" "]{"
                     ))
        (setq tex-string (string-replace ele "" tex-string)))
      (or
       ;; (memq 103 (string-to-list tex-string)) ;; g
       (memq 106 (string-to-list tex-string)) ;; j
       (memq 112 (string-to-list tex-string)) ;; p
       (memq 113 (string-to-list tex-string)) ;; q
       (memq 121 (string-to-list tex-string)) ;; y
       (string-match "\\xi" tex-string)       ;; ξ
       (string-match "\\zeta" tex-string)     ;; ζ
       )))

  (defun my/latex-tail-pun-p ()
    (memq 44 (string-to-list (my/org-latex--get-tex-string))))

  (defun my/latex-fragment-subscript-p ()
    "Return `t' if '_' in current LaTeX fragment."
    (and
     (not (string-match-p "_\{.*?\}\(" (my/org-latex--get-tex-string)))
     (memq 95 (string-to-list (my/org-latex--get-tex-string)))
         ))

  (defun my/latex-fragment-script-p ()
    "Return `t' if both '_' &  '^' in current LaTeX fragment."
     (and (memq 94 (string-to-list (my/org-latex--get-tex-string)))
          (memq 95 (string-to-list (my/org-latex--get-tex-string)))))

  (defun my/latex-fragment-frac-p ()
    "Return `t' if contain frac in current LaTeX fragment."
     (string-match "frac" (my/org-latex--get-tex-string)))

  (defun my/latex-fragment-tail-and-subscript-p ()
    "Return `t' if contain frac in current LaTeX fragment."
    (and
     (my/latex-fragment-subscript-p)
     (my/latex-tail-latin-p)
     ))

  (defun my/latex-fragment-cfrac-and-subscript-p ()
    "Return `t' if contain frac in current LaTeX fragment."
    (and
     (my/latex-fragment-subscript-p)
     (string-match "cfrac" (my/org-latex--get-tex-string))
     ))

  (defun my/latex-fragment-floor-p ()
    "Return `t' if contain frac in current LaTeX fragment."
    (string-match "lfloor" (my/org-latex--get-tex-string)))

  (defun my/latex-fragment-matrix-p ()
    "Return `t' if contain frac in current LaTeX fragment."
    (or
     (string-match "matrix}" (my/org-latex--get-tex-string))
     (string-match "array}" (my/org-latex--get-tex-string))))


  (defun my/log-subscript-fragment-p ()
    (string-match "log^\{" (my/org-latex--get-tex-string)))

  (defun my/latex-fragment-bracket-p ()
    "Return `t' if '(' in current LaTeX fragment."
    (let (tex-string)
      (setq tex-string (my/org-latex--get-tex-string))
      (dolist (ele '("^{(" "_{(" "_{" "^{"))
        (setq tex-string (string-replace ele "" tex-string)))
      (or
       (memq 40 (string-to-list tex-string))
       (memq 123 (string-to-list tex-string)))))

  (defun my/latex-fragment-radical-p ()
    (string-match "sqrt" (my/org-latex--get-tex-string)))

  (defun org--make-preview-overlay (beg end image &optional imagetype)
    "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
    (setq my/position 100)
    (cond
          ((my/latex-fragment-matrix-p)
           (setq my/position 70))
          ((my/latex-fragment-cfrac-and-subscript-p)
           (setq my/position 59))
          ((my/latex-fragment-frac-p)
           (setq my/position 67))
          ((my/latex-fragment-script-p)
           (setq my/position 76))
          ((my/latex-fragment-subscript-p)
           (setq my/position 82))
          ((my/latex-tail-latin-p)
           (setq my/position 82))
          ((my/latex-fragment-radical-p)
           (setq my/position 89))
          ((my/latex-fragment-bracket-p)
           (setq my/position 87))
          ((my/latex-tail-pun-p)
           (setq my/position 83))
          ((my/latex-fragment-floor-p)
           (setq my/position 83))
          ((my/log-subscript-fragment-p)
           (setq my/position 83))
          ((my/latex-fragment-superscript-p)
           (setq my/position 100))
          )
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
  (defun nyoom-org-latex-fragment-justify (justification)
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

  (defun nyoom-org-latex-fragment-justify-advice (_ _ _ _)
    "After advice function to justify fragments."
    (nyoom-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'center)))

  (advice-add 'org--make-preview-overlay :after 'nyoom-org-latex-fragment-justify-advice))

;;; Org Image ────────────────────────────────────────────────────────────────────────
(after! org
  (defun nyoom-get-image-dimensions-imk (Filepath)
    "Returns a image file's width and height as a vector.
This function requires “magick identify” shell command.
See also: `nyoom-get-image-dimensions'."
    (let (($widthHeight
           (split-string
            (shell-command-to-string
             (concat "magick identify -format \"%w %h\" " Filepath)))))
      (vector
       (string-to-number (elt $widthHeight 0))
       (string-to-number (elt $widthHeight 1)))))

  (defun nyoom-get-image-dimensions (Filepath)
    "Returns a vector [width height] of a image's dimension.
The elements are integer datatype.
Support png jpg svg gif and any image type emacs supports.
If it's svg, and dimension cannot be determined, it returns [0 0].
If it's webp, calls `nyoom-get-image-dimensions-imk'.

URL `http://xahlee.info/emacs/emacs/elisp_image_tag.html'
Version 2017-01-11 2021-09-01"
    (let ($x $y)
      (cond
       ;; ((string-match "\.svg$" Filepath)
       ;;  (progn
       ;;    (with-temp-buffer
       ;;      ;; hackish. grab the first occurence of width height in file
       ;;      (insert-file-contents Filepath)
       ;;      (goto-char (point-min))
       ;;      (when (re-search-forward "width=\"\\([0-9]+\\).*\"" nil 1)
       ;;        (setq $x (match-string-no-properties 1 )))
       ;;      (goto-char (point-min))
       ;;      (if (re-search-forward "height=\"\\([0-9]+\\).*\"" nil 1)
       ;;          (setq $y (match-string-no-properties 1 ))))
       ;;    (if (and $x $y)
       ;;        (vector (string-to-number $x) (string-to-number $y))
       ;;      (cons 0 0))))
       ((string-match "\.webp$" Filepath)
        (nyoom-get-image-dimensions-imk Filepath))
       (t
        (let ($xy )
          (progn
            (clear-image-cache t)
            (setq $xy (image-size
                       (create-image
                        (if (file-name-absolute-p Filepath)
                            Filepath
                          (concat default-directory Filepath)))
                       t)))
          $xy)))))

  (defun nyoom-image-actual-width (org-link)
    (car (nyoom-get-image-dimensions (org-element-property :path org-link))))

  (defun nyoom-org-display-inline-images (&optional include-linked refresh beg end)
    "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link, i.e., it must have an explicit \"file\" type.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
    (interactive "P")
    (when (display-graphic-p)
      (when refresh
        (org-remove-inline-images beg end)
        (when (fboundp 'clear-image-cache) (clear-image-cache)))
      (let ((end (or end (point-max))))
        (org-with-point-at (or beg (point-min))
          (let* ((case-fold-search t)
                 (file-extension-re (image-file-name-regexp))
                 (link-abbrevs (mapcar #'car
                                       (append org-link-abbrev-alist-local
                                               org-link-abbrev-alist)))
                 ;; Check absolute, relative file names and explicit
                 ;; "file:" links.  Also check link abbreviations since
                 ;; some might expand to "file" links.
                 (file-types-re
                  (format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
                          (if (not link-abbrevs) ""
                            (concat "\\|" (regexp-opt link-abbrevs))))))
            (while (re-search-forward file-types-re end t)
              (let* ((link (org-element-lineage
                            (save-match-data (org-element-context))
                            '(link) t))
                     (linktype (org-element-property :type link))
                     (inner-start (match-beginning 1))
                     (path
                      (cond
                       ;; No link at point; no inline image.
                       ((not link) nil)
                       ;; File link without a description.  Also handle
                       ;; INCLUDE-LINKED here since it should have
                       ;; precedence over the next case.  I.e., if link
                       ;; contains filenames in both the path and the
                       ;; description, prioritize the path only when
                       ;; INCLUDE-LINKED is non-nil.
                       ((or (not (org-element-property :contents-begin link))
                            include-linked)
                        (and (or (equal "file" linktype)
                                 (equal "attachment" linktype))
                             (org-element-property :path link)))
                       ;; Link with a description.  Check if description
                       ;; is a filename.  Even if Org doesn't have syntax
                       ;; for those -- clickable image -- constructs, fake
                       ;; them, as in `org-export-insert-image-links'.
                       ((not inner-start) nil)
                       (t
                        (org-with-point-at inner-start
                          (and (looking-at
                                (if (char-equal ?< (char-after inner-start))
                                    org-link-angle-re
                                  org-link-plain-re))
                               ;; File name must fill the whole
                               ;; description.
                               (= (org-element-property :contents-end link)
                                  (match-end 0))
                               (match-string 2)))))))
                (when (and path (string-match-p file-extension-re path))
                  (let ((file (if (equal "attachment" linktype)
                                  (progn
                                    (require 'org-attach)
                                    (ignore-errors (org-attach-expand path)))
                                (expand-file-name path))))
                    (when (and file (file-exists-p file))
                      (let ((width (or
                                    (org-display-inline-image--width link)
                                    (if (> (nyoom-image-actual-width link) (round (* (window-max-chars-per-line) 9 0.8)))
                                        (round (* (window-max-chars-per-line) 9 0.6))
                                      (nyoom-image-actual-width link))
                                    ))
                            (old (get-char-property-and-overlay
                                  (org-element-property :begin link)
                                  'org-image-overlay)))
                        (if (and (car-safe old) refresh)
                            (image-flush (overlay-get (cdr old) 'display))
                          (let ((image (org--create-inline-image file width)))
                            (when image
                              (let ((ov (make-overlay
                                         (org-element-property :begin link)
                                         (progn
                                           (goto-char
                                            (org-element-property :end link))
                                           (skip-chars-backward " \t")
                                           (point))))
                                    (shift (- (org-element-property :begin link) (line-beginning-position)))
                                    (justify (org-display-inline-image--justify link))
                                    (normal-size (round (* (window-max-chars-per-line) 0.8)))
                                    ;; (text-px-unit
                                    ;;  (round (/ (car (window-text-pixel-size)) (* 1.0 (window-text-width)))))
                                    )
                                (message "%s" width)
                                ;; (message justify)
                                ;; FIXME: See bug#59902.  We cannot rely
                                ;; on Emacs to update image if the file
                                ;; has changed.
                                (image-flush image)
                                (overlay-put ov 'display image)
                                (overlay-put ov 'face 'default)
                                (overlay-put ov 'org-image-overlay t)

                                (setq space-left (- (window-max-chars-per-line)
                                                    (round (/ width 9)))
                                      offset (floor (cond
                                                     ((string= justify "center")
                                                      (- (/ space-left 2) shift))
                                                     (t
                                                      0))))

                                (when (>= offset 0)
                                  (overlay-put ov 'before-string (make-string offset ?\ )))

                                (overlay-put
                                 ov 'modification-hooks
                                 (list 'org-display-inline-remove-overlay))
                                (when (boundp 'image-map)
                                  (overlay-put ov 'keymap image-map))
                                (push ov org-inline-image-overlays))))))))))))))))

  (defun org-display-inline-image--justify (link)
    (let ((justify "center"))
        (let* ((case-fold-search t)
               (par (org-element-lineage link '(paragraph)))
               (attr-re "^[ \t]*#\\+attr_.*?: +.*?:justify +\\(\\S-+\\)")
               (par-end (org-element-property :post-affiliated par))
               ;; Try to find an attribute providing a :justify.
               (attr-justify
                (when (and par (org-with-point-at
                                   (org-element-property :begin par)
                                 (re-search-forward attr-re par-end t)))
                  (match-string 1)))
               (justify
                (cond
                 ;; Convert numeric justifys to numbers, converting percentages.
                 ((string= attr-justify "left")
                  "left")
                 (t "center"))))
          justify)))

  (advice-add 'org-display-inline-images :override #'nyoom-org-display-inline-images))

;;; TODO ────────────────────────────────────────────────────────────────────────
(defface space-lock
  '((t (:font-family "Space Mono"
        :height 160
        :avgwidth 180
        :spacing 100
        )))
  "Org mode todo face"
  :group 'org-face)

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
    ;; ("^\\(-\\)$"
    ;;  (0 (list 'face nil 'display (dash-to-hyphen (match-string 1)))))
    ("^\\(-\\{1,\\}\\)$"
     ;; (0 (list 'face 'fira-lock 'display (dash-to-hyphen (match-string 1)))))
     (0 (list 'face nil 'display (dash-to-hyphen (match-string 1)))))
    ))

(defun dash-to-hyphen (value)
  (format "%s" (make-string (length value) #x2014)))

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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  ;; (let (org-log-done org-log-states)   ; turn off logging
  (let (org-log-done)   ; turn off logging
    (message (format "%s" n-done))
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; org mode motion
;; (with-eval-after-load 'evil
;;   (defun narrow-p ()
;;     "Return t if a buffer is narrowed"
;;     (not (equal (- (point-max) (point-min)) (buffer-size))))
;;
;;   (defun what-org ()
;;     "Get the org-element-type at point."
;;     (interactive)
;;     (require 'org-element)
;;     (message "element type of %s, parent type of %s"
;;              (org-element-type (org-element-at-point))
;;              (org-element-property :language (org-element-at-point))))
;;
;;   (defun toggle-narrow ()
;;     (interactive)
;;     (if (narrow-p)
;;         (widen)
;;       (org-narrow-to-subtree)))
;;
;;   (define-key evil-normal-state-map (kbd "C-o") 'toggle-narrow)
;;   (define-key evil-normal-state-map (kbd "s-o") 'what-org))

;; quickly edit org mode with normal mode
(with-eval-after-load 'evil
  (defun cycle-format ()
    (interactive)
    (require 'org-element)
    (setq lang (org-element-property :language (org-element-at-point)))
    (message "src block lang %s" lang)
    ;; (if (string-equal lang "java")
    ;;     (progn
    ;;       (org-edit-special)
    ;;       (kill-region (point-min) (point-max))
    ;;       ;; (shell-command "pbpaste | google-java-format --aosp - | pbcopy")
    ;;       (insert (shell-command-to-string "pbpaste | google-java-format --aosp -"))
    ;;      ))

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

  (define-key evil-normal-state-map (kbd "s-b") 'quickb)
  (define-key evil-normal-state-map (kbd "s-,") 'quickv)
  (define-key evil-normal-state-map (kbd "s-;") 'quickc)
  (define-key evil-normal-state-map (kbd "s-f") 'cycle-format)
)

;; edit org mode visual mode
(with-eval-after-load 'evil
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
    (kbd "/") 'wrap-with-italic))

;; Referenced: https://www.reddit.com/r/emacs/comments/b8jqor/making_orgmode_narrowing_as_intuitive_as_workflow/
(with-eval-after-load 'evil
  (defun vimacs/org-narrow-to-subtree
      ()
    (interactive)
    (let ((org-indirect-buffer-display 'current-window))
      (if (not (boundp 'org-indirect-buffer-file-name))
          (let ((above-buffer (current-buffer))
                (org-filename (buffer-file-name)))
            (org-tree-to-indirect-buffer (1+ (org-current-level)))
            (setq-local org-indirect-buffer-file-name org-filename)
            (setq-local org-indirect-above-buffer above-buffer))
        (let ((above-buffer (current-buffer))
              (org-filename org-indirect-buffer-file-name))
          (org-tree-to-indirect-buffer (1+ (org-current-level)))
          (setq-local org-indirect-buffer-file-name org-filename)
          (setq-local org-indirect-above-buffer above-buffer)))))

  (defun vimacs/org-widen-from-subtree
      ()
    (interactive)
    (let ((above-buffer org-indirect-above-buffer)
          (org-indirect-buffer-display 'current-window))
      (kill-buffer)
      (switch-to-buffer above-buffer)))

  (define-key evil-normal-state-map (kbd "C-o") 'vimacs/org-narrow-to-subtree)
  (define-key evil-normal-state-map (kbd "s-o") 'vimacs/org-widen-from-subtree))

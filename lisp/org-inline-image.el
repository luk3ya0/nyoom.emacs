(require 'org)
(require 'dash)
(require 'f)

(defgroup org-inline-image ()
  "Inline images into org-mode buffers."
  :group 'org
  :prefix "org-inline-image-")

(defcustom org-inline-image-root "/tmp/org-inline-image/"
  "Root directory where temp files are stored."
  :type 'directory
  :group 'org-inline-image)

(defcustom org-inline-image-resolvers '(org-inline-image--regexp-resolver identity)
  "List of functions which resolve URLs.

Each function should take one input argument.

The input is an arbitrary URL.

The output is a direct URL to the image resource (presumably
related to the input URL).  The first non-nil result is used."
  :type 'hook
  :options '(org-inline-image--regexp-resolver identity)
  :group 'org-inline-image)

(defcustom org-inline-image-regexp-resolver-alist
  '(("deviantart\\.com/art/" . org-inline-image--resolve-deviantart-image)
    ("//\\(www\\.\\)?imgur\\.com" . org-inline-image--resolve-imgur-image))
  "Alist maping a regular expressions to a resolver.

Resolver should be a function mapping the input URL to an URL
pointing to an image resource (presumably related to the input
URL).

This is used in `org-inline-image-regexp-resolver'."
  :type '(alist
          :key-type regexp
          :value-type function)
  :group 'org-inline-image)

;; TODO: add key to resize/fill/refresh
(defvar org-inline-image-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'org-inline-image-animate)
    (define-key map (kbd "h") 'org-inline-image-hide)
    map)
  "Keymap active when point is on the image.")

(defun org-inline-image--regexp-resolver (input)
  "Resolve the URL by regexp.

Map the INPUT URL to an image resource associated with this URL.

This function uses the `org-inline-image-regexp-resolver-alist'
to convert INPUTs to outputs."
  (let ((resolvers org-inline-image-regexp-resolver-alist))
    (while (and (caar resolvers)
                (not (string-match-p
                      (caar resolvers)
                      input)))
      (pop resolvers))
    (when resolvers
      (funcall (cdar resolvers) input))))

(defun org-inline-image--resolve-deviantart-image (input)
  "Resolve deviantart.com/art/ URL."
  (let ((hexified (url-hexify-string input)))
    (save-match-data
      (with-current-buffer
          (url-retrieve-synchronously (concat "http://backend.deviantart.com/oembed?url=" hexified))
        (goto-char (point-min))
        (when (re-search-forward "\"url\":\"\\(.*?\\)\"")
          (match-string 1))))))

(defun org-inline-image--resolve-imgur-image (input)
  "Resolve imgur.com URL."
  (unless (string-match-p "gallery" input)
    (with-current-buffer (url-retrieve-synchronously input)
      (goto-char (point-min))
      (when (re-search-forward "<link rel=\"image_src\" href=\"\\(.*?\\)\"/>")
        (match-string 1)))))

(defun org-inline-image--create-root-maybe ()
  "Create root directory if it doesn't exist yet."
  (unless (file-exists-p org-inline-image-root)
    (make-directory org-inline-image-root t)))

(defun org-inline-image--get-image-props (file)
  "Return image properties for FILE."
  `(image :type ,(image-type file)
          :file ,file
          :relief 0
          :margin 0))

(defun org-inline-image--get-current-image ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'org-image-overlay) t) (overlays-at (point)))))

(defun org-inline-image--get (prop)
  "Return the value of property PROP for image under point."
  (let ((image (org-inline-image--get-current-image)))
    (when image
      (overlay-get image prop))))

;; TODO: cache downloaded images?
;; TODO: add support for local images
;; TODO: add support to "pop image to separate buffer/window"
;;;###autoload
(defun org-inline-image ()
  "Inline an image."
  (interactive)
  (org-inline-image--create-root-maybe)
  (-when-let (link-data (org-inline-image--get-link))
    (let* ((resolved-link (run-hook-with-args-until-success
                           'org-inline-image-resolvers
                           (plist-get link-data :link)))
           (name (concat org-inline-image-root (f-filename resolved-link))))
      (when (url-copy-file resolved-link name)
        (let ((ov (make-overlay (plist-get link-data :beg) (plist-get link-data :end)))
              ;; TODO: replace with `create-image'?
              ;; this can also support automatic resizing
              (image-props (org-inline-image--get-image-props name)))
          (overlay-put ov 'type 'oii)
          (overlay-put ov 'display image-props)
          (overlay-put ov 'face 'default)
          (overlay-put ov 'original-file name)
          (overlay-put ov 'keymap org-inline-image-keymap)
          (when (image-multi-frame-p image-props)
            (image-animate image-props))
          (goto-char (plist-get link-data :beg)))))))


;; TODO: make removing optional?
(defun org-inline-image-hide ()
  "Hide the inlined image at point.

The file is also removed from the filesystem.  Repeated inlining
will re-download the file."
  (interactive)
  (let ((ov (org-inline-image--get-current-image))
        (original-file (org-inline-image--get 'original-file)))
    (delete-overlay ov)
    (delete-file original-file)))

(defun org-inline-image-animate ()
  "Animate the image if it's possible."
  (interactive)
  (let ((image-props (org-inline-image--get 'display)))
    (when (image-multi-frame-p image-props)
      (image-animate image-props))))

(defun org-inline-image-animate-auto ()
  (interactive)
  (when (eq 'org-mode major-mode)
    (while-no-input
      (run-with-idle-timer 0.3 nil 'org-inline-image-animate))))

(defun org-inline-image--get-link ()
  "Get link at point."
  (let (beg end link)
    (cond
     ((org-in-regexp org-bracket-link-regexp)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq link (match-string 1)))
     ((org-in-regexp org-angle-link-re)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq link (match-string 1)))
     ((org-in-regexp org-plain-link-re)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq link (match-string 0))))
    (list :beg beg :end end :link (plist-get (get-text-property 1 'htmlize-link link) :uri))))

(setq org-inline-image--get-current-image (byte-compile 'org-inline-image--get-current-image))
(setq org-inline-image-animate  (byte-compile 'org-inline-image-animate ))
(add-hook 'post-command-hook 'org-inline-image-animate-auto)

(provide 'org-inline-image)

(require 'svg-lib)

(defvar svg-font-lock-keywords
  `(
    ("\\:EMACS\\:"
     (0 (list 'face nil 'display (svg-font-lock-intent (match-string 1)))))
    ("\\[\\([0-9]\\{1,3\\}\\)%\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_percent (match-string 1)))))
    ("\\[\\([0-9]+/[0-9]+\\)\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_count (match-string 1)))))))

(defun svg-font-lock-intent (value)
  (svg-lib-icon "gnuemacs" nil :collection "simple"
		:stroke 0 :scale 1 :padding 0)
  )

(defun svg-font-lock-progress_percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 12)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-font-lock-progress_count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                :margin 0 :stroke 2 :radius 3 :padding 2 :width 12)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(provide 'org-progress)

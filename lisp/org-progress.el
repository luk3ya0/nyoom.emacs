(require 'svg-lib)

(defvar svg-font-lock-keywords
  `(
    ;;("^\*\\{1\\}\s"
    ;; (0 (list 'face nil 'display (svg-font-lock-level-one (match-string 1)))))
    ;;("^\*\\{2\\}\s"
    ;; (0 (list 'face nil 'display (svg-font-lock-level-two (match-string 1)))))
    ;;("^\*\\{3\\}\s"
    ;; (0 (list 'face nil 'display (svg-font-lock-level-three (match-string 1)))))
    ;;("^\*\\{4\\}\s"
    ;; (0 (list 'face nil 'display (svg-font-lock-level-four (match-string 1)))))
    ;;("^\*\\{5\\}\s"
    ;; (0 (list 'face nil 'display (svg-font-lock-level-five (match-string 1)))))
    ("EMACS"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "gnuemacs"))))
    ("DESIGN"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "puzzle-outline"))))
    ("INTENT"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "comment-processing-outline"))))
    ("PROBLEM"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "emoticon-frown-outline"))))
    ("SOLUTION"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "emoticon-happy-outline"))))
    ("STRUCTURE"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "sitemap-outline"))))
    ("PSEUDO"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "pound"))))
    ("APP"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "lightbulb-on-outline"))))
    ("BUG"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "bug"))))
    ("BOLT"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "flash-outline"))))
    ("IMPL"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "clipboard-check-outline"))))
    ("BALANCE"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "scale-balance"))))
    ("PROS"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "check"))))
    ("CONS"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "close-thick"))))
    ("RELA"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "swap-horizontal"))))
    ("REAL"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "car-side"))))
    ("CODE"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "xml"))))
    ("EXTRA"
     (0 (list 'face nil 'display (svg-font-lock-material (match-string 1) "gift-outline"))))
    ("JAVA"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "java"))))
    ("PYTHON"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "python"))))
    ("TYPESCRIPT"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "typescript"))))
    ("GO"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "go"))))
    ("RUST"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "rust"))))
    ("FLINK"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "apacheflink"))))
    ("KAFKA"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "apachekafka"))))
    ("HADOOP"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "apachehadoop"))))
    ("SPRING"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "spring"))))
    ("TOMCAT"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "apachetomcat"))))
    ("NGINX"
     (0 (list 'face nil 'display (svg-font-lock-simple (match-string 1) "nginx"))))
    ("TODO"
     (0 (list 'face nil 'display (svg-font-lock-todo (match-string 1)))))
    ("DONE"
     (0 (list 'face nil 'display (svg-font-lock-done (match-string 1)))))
    ("\\[\\([0-9]\\{1,3\\}\\)%\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_percent (match-string 1)))))
    ("\\[\\([0-9]+/[0-9]+\\)\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_count (match-string 1)))))))

(defun svg-font-lock-level-one (value)
  (format "%s " "✺"))

(defun svg-font-lock-level-two (value)
  (format "%s " "✦✦"))

(defun svg-font-lock-level-three (value)
  (format "%s " "✱✱✱"))

(defun svg-font-lock-level-four (value)
  (format "%s " "✸✸✸✸"))

(defun svg-font-lock-level-five (value)
  (format "%s " "★★★★★"))

(defun svg-font-lock-material (value actual)
  (svg-lib-icon actual nil :collection "material"
		:stroke 0 :scale 1 :padding 0))

(defun svg-font-lock-simple (value actual)
  (svg-lib-icon actual nil :collection "simple"
		:stroke 0 :scale 1 :padding 0))

(defun svg-font-lock-done (value)
  (svg-lib-button "checkbox-multiple-marked" "DONE" nil
  		  :font-family "Roboto Mono"
  		  :font-weight 700
  		  :stroke 0 :background "#673AB7" :foreground "white"))

(defun svg-font-lock-todo (value)
  (svg-lib-button "checkbox-multiple-blank" "TODO" nil
  		  :font-family "Roboto Mono"
  		  :font-weight 700
  		  :stroke 0 :background "#548B54" :foreground "white"))


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

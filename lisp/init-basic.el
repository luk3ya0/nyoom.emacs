(setq mac-option-modifier  'meta
      mac-command-modifier 'super)

(global-set-key [(super a)] #'mark-whole-buffer)
(global-set-key [(super v)] #'yank)
(global-set-key [(super c)] #'kill-ring-save)
(global-set-key [(super q)] #'save-buffers-kill-emacs)
(global-set-key [(super s)] #'save-buffer)
(global-set-key [(super l)] #'goto-line)
(global-set-key [(super w)] #'kill-this-buffer)
(global-set-key [(super z)] #'undo)

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  (gcmh-mode 1))

(provide 'init-basic)

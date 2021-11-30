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
(global-set-key [(super \[)] #'previous-buffer)
(global-set-key [(super \])] #'next-buffer)

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  (gcmh-mode 1))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t            ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-="   . default-text-scale-increase)
         ("s--"   . default-text-scale-decrease)
         ("s-0"   . default-text-scale-reset)))

(provide 'init-basic)

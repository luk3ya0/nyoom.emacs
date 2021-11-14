(setq mac-option-modifier  'meta
      mac-command-modifier 'super)

(bind-keys ([(super a)] . mark-whole-buffer)
           ([(super c)] . kill-ring-save)
           ([(super l)] . goto-line)
           ([(super q)] . save-buffers-kill-emacs)
           ([(super s)] . save-buffer)
           ([(super v)] . yank)
           ([(super w)] . delete-frame)
           ([(super z)] . undo))

(provide 'init-mac)

;; Speed up startup
(setq auto-mode-case-fold nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup         nil)
(setq inhibit-startup-screen            t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message           t)
(setq initial-scratch-message           nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0)                  default-frame-alist)
(push '(tool-bar-lines . 0)                  default-frame-alist)
(push '(vertical-scroll-bars . nil)          default-frame-alist)
(push '(width  . 121)                        default-frame-alist)
(push '(min-width  . 1)                      default-frame-alist)
(push '(height . 64)                         default-frame-alist)
(push '(min-height . 1)                      default-frame-alist)
(push '(internal-border-width . 24)          default-frame-alist)
(push '(font . "Roboto Mono Light 15")       default-frame-alist)
(push '(left-fringe    . 1)                  default-frame-alist)  
(push '(right-fringe   . 1)                  default-frame-alist)
;;(push `(alpha . ,'(90 . 90))     default-frame-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here

;;; unannoy.el --- disable Emacs' annoying bits

;;; Code:

(setf backup-inhibited t
      auto-save-default nil
      inhibit-startup-message t
      initial-scratch-message nil
      wdired-allow-to-change-permissions t
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32))

;; GUIs are for newbs
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Too distracting
(blink-cursor-mode -1)

;; Don't jerk me around
(electric-indent-mode -1)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always use the one true encoding
(prefer-coding-system 'utf-8-unix)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      vc-follow-symlinks t)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

(provide 'unannoy)

;;; unannoy.el ends here

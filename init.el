(add-to-list 'load-path "~/.emacs.d")
(random t)

(require 'cl)
(require 'memoize)
(require 'htmlize)
(require 'highlight-tags-mode)
(require 'dired+)
(require 'markdown-mode)
(require 'my-funcs) ; custom functions

;; Turn off the newbie crap
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq backup-inhibited t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq wdired-allow-to-change-permissions t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dabbrev-case-distinction nil)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Web server
(add-to-list 'load-path "~/.emacs.d/emacs-httpd")
(require 'httpd)

;; Winner mode
(require 'winner)
(winner-mode 1)
(windmove-default-keybindings)

;; Git
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Printing
(require 'ps-print)
(setq ps-print-header nil)

;; GLSL
(require 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))

;; groff
(add-to-list 'auto-mode-alist '("\\.mom\\'" . nroff-mode))

;; ERC
(setq erc-nick "skeeto")

;; C
(require 'cc-mode)
(add-to-list 'c-default-style '(c-mode . "k&r"))

;; Parenthesis
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode 1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode 1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode 1)))
(add-hook 'ielm-mode-hook             (lambda () (paredit-mode 1)))
(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each ielm prompt with a paredit pair.."
  (paredit-open-round))
(show-paren-mode 1)
(require 'parenface)
(set-face-foreground 'paren-face "gray30")
;(set-face-foreground 'paren-face "gray50")
;(set-face-foreground 'paren-face "gray60")

;; Ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t) ; Old habits die hard!
(setq ido-everywhere t)
(ido-mode 1)

;; Smex
(add-to-list 'load-path "~/.emacs.d/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Set the color theme
(when (boundp 'custom-safe-themes)
  (setq custom-safe-themes '(default
			      "7a6bbc6f0af4a34f3e2bad5d334c045350d0705a"
			      "28b1914fbb2b71fd5d597783dd72c4ccaeb2cf19"
			      "649ccffdb140fcd021013f6297dedeb313b74fa5")))
(add-to-list (if (boundp 'custom-theme-load-path)
		 'custom-theme-load-path
	       'load-path)
	     "~/.emacs.d/themes/")
(load-theme 'wombat)
;(set-face-attribute 'default nil :height 100)
;(set-frame-parameter (selected-frame) 'alpha 80)
;(set-default-font "Inconsolata-12")

;; Java
(add-to-list 'load-path "~/.emacs.d/emacs-java")
(require 'java-mode-plus)
(require 'java-docs)
(setq browse-url-browser-function 'browse-url-firefox)
(add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil)))
(java-mode-short-keybindings)

;; YASnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/yasnippet-java")
(yas/load-directory "~/.emacs.d/emacs-java/snippets")

;; Custom bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-k" 'compile)
(global-set-key [f1] (lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key [f2] (expose (apply-partially 'revert-buffer nil t)))

;; Dedicated windows
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key [pause] 'toggle-current-window-dedication)

;; Make bindings like my java-mode-plus stuff
(defmacro make-bind (key target)
  `(lexical-let ((target ,target))
     (global-set-key ,key (lambda () (interactive)
                            (compile (concat "make " (if (symbolp target)
                                                         (symbol-name target)
                                                       target)))))))

(make-bind (kbd "C-x c") "")
(make-bind (kbd "C-x r") 'run)
(make-bind (kbd "C-x C") 'clean)

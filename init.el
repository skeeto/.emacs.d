(add-to-list 'load-path "~/.emacs.d")
(random t)

(require 'cl)
(require 'memoize)
(require 'htmlize)
(require 'highlight-tags-mode)
(require 'dired+)
(require 'my-funcs) ; custom functions

;; Turn off the newbie crap
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq wdired-allow-to-change-permissions t)
(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dabbrev-case-distinction nil)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)
(mapatoms (lambda (sym)
            (if (get sym 'disabled)
                (put sym 'disabled nil))))

;; tramp
(require 'tramp)  ; manual require due to Emacs 23 bug
(require 'tramp-cache)
(setq tramp-persistency-file-name
      (concat temporary-file-directory "tramp-" (user-login-name)))

;; Use proper whitespace
(require 'whitespace)
(setq-default indent-tabs-mode nil)
(defcustom do-whitespace-cleanup t "Perform whitespace-cleanup on save.")
(make-variable-buffer-local 'do-whitespace-cleanup)
(add-hook 'before-save-hook
          (lambda ()
            ;; turn off and on to work around Emacs bug #4069
            (whitespace-turn-on)
            (whitespace-turn-off)
            (if do-whitespace-cleanup
                (whitespace-cleanup))))
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

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

;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(define-key markdown-mode-map (kbd "<tab>") nil) ; fix for YASnippet

;; Jekyll
(require 'jekyll)
(global-set-key (kbd "C-c n") 'jekyll/new-post)
(setq jekyll-home "~/src/skeeto.github.com/")
(when (file-exists-p jekyll-home)
  (setq httpd-root (concat jekyll-home "_site"))
  (condition-case e
      (progn
        (httpd-start)
        (jekyll/start))
    ('error (message (format "notice: httpd failed: %s" e)))))

;; Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

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
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode)) ; OpenCL

;; groff
(add-to-list 'auto-mode-alist '("\\.mom\\'" . nroff-mode))

;; ERC (only set it for me)
(if (eq 0 (string-match "wello" (user-login-name)))
    (setq erc-nick "skeeto"))

;; C (and fix Emacs' incorrect k&r indentation)
(require 'cc-mode)
(setcdr (assq 'c-basic-offset (cdr (assoc "k&r" c-style-alist))) 4)
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

;; imenu
(require 'idomenu)
(global-set-key (kbd "C-c c") 'idomenu)

;; Set the color theme
(if (>= emacs-major-version 24)
    (load-theme 'wombat t))
;(set-face-attribute 'default nil :height 100)
;(set-frame-parameter (selected-frame) 'alpha 80)
;(set-default-font "Inconsolata-12")

;; Java
(add-to-list 'load-path "~/.emacs.d/emacs-java")
(require 'java-mode-plus)
(require 'java-docs)
(if (executable-find "firefox")
    (setq browse-url-browser-function 'browse-url-firefox))
(java-mode-short-keybindings)
(apply 'java-docs
       (remove-if-not 'file-directory-p
                      (directory-files "~/.emacs.d/javadoc" t "^[^.].*$")))

;; YASnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/yasnippet-java")
(yas/load-directory "~/.emacs.d/emacs-java/snippets")

;; mark-multiple
(add-to-list 'load-path "~/.emacs.d/mark-multiple")
(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)

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
(defmacro compile-bind (map key builder target)
  "Define a key binding for a build system target (i.e. make,
ant, scons) in a particular keymap."
  `(define-key ,map ,key
     (lambda (n)
       (interactive "p")
       (let* ((buffer-name (format "*compilation-%d*" n))
              (compilation-buffer-name-function (lambda (x) buffer-name)))
         (save-buffer)
         (compile (format "%s %s" ,builder ,target) t)))))

(defmacro compile-bind* (map builder keys/fns)
  "Create several compile-bind bindings in a row."
  `(progn
     ,@(loop for (key fn) on keys/fns by 'cddr
             collecting `(compile-bind ,map (kbd ,key) ,builder ,fn))))

(compile-bind*	; example of compile-bind*, global make bindings
 (current-global-map)
 'make ("C-x c" ""
        "C-x r" 'run
        "C-x C" 'clean))

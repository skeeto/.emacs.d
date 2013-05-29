(add-to-list 'load-path "~/.emacs.d")

;;; Turn off the annoying crap immediately
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq wdired-allow-to-change-permissions t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search nil)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)
(mapatoms (lambda (sym)
            (if (get sym 'disabled)
                (put sym 'disabled nil))))

;;; Packages

(require 'package)
(require 'package-helper)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Install packages not needing configuration
(with-package (impatient-mode lua-mode memoize rdp))

;; Load local "packages"
(require 'imgur)
(require 'misc)

;;; Custom global bindings

(with-package* utility
  (global-set-key (kbd "C-S-j") 'join-line)
  (global-set-key "\M-g" 'goto-line)
  (global-set-key "\C-x\C-k" 'compile)
  (global-set-key [f2] (expose #'revert-buffer nil t))
  (global-set-key [f5] (lambda () (interactive) (mapatoms 'byte-compile))))

;;; auto-mode-alist entries

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))

;;; Individual package configurations

(with-package (lisp-mode utility)
  (defalias 'lisp-interaction-mode 'emacs-lisp-mode)
  (defun ielm-repl ()
    (interactive)
    (pop-to-buffer (get-buffer-create "*ielm*"))
    (ielm))
  (defun ert-silently ()
    (interactive)
    (ert t))
  (define-key emacs-lisp-mode-map (kbd "C-x r") (expose #'ert t))
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm-repl)
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  (font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\<\\(\\(?:ert-\\)?deftest\\)\\> +\\([^ ()]+\\)"
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face)))))

(with-package* time
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-24hr-format t)
  (display-time-mode t))

(with-package comint
  (message "comint loaded: %s" (featurep 'comint))
  (setq comint-prompt-read-only t
        comint-history-isearch t)
  (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
  (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
  (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
  (define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward))

(with-package tramp
  (setq tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

(with-package* whitespace-cleanup
  (setq-default indent-tabs-mode nil))

(with-package (simple utility)
  ;; disable so I don't use it by accident
  (define-key visual-line-mode-map (kbd "M-q") (expose (lambda ()))))

(with-package* uniquify
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(with-package* winner
  (winner-mode 1)
  (windmove-default-keybindings))

(with-package magit-autoloads
  (setq vc-display-status nil)
  (global-set-key "\C-xg" 'magit-status))

(with-package markdown-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("pentadactyl.txt$" . markdown-mode))
  (setq sentence-end-double-space nil))

(with-package markdown-mode
  (define-key markdown-mode-map (kbd "<tab>") nil)) ; fix for YASnippet

(with-package* (simple-httpd jekyll)
  (setq jekyll-home "~/src/skeeto.github.com/")
  (when (file-exists-p jekyll-home)
    (setq httpd-root (concat jekyll-home "_site"))
    (ignore-errors
      (httpd-start)
      (jekyll/start)))
  (defservlet robots.txt text/plain ()
    (insert "User-agent: *\nDisallow: /\n"))
  (defservlet uptime "text/plain" ()
    (princ (emacs-uptime))))

(with-package js2-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(with-package js2-mode
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
  (setq js2-skip-preprocessor-directives t)
  (setq-default js2-additional-externs
                '("$" "unsafeWindow" "localStorage" "jQuery"
                  "setTimeout" "setInterval" "location" "skewer"
                  "console" "phantom")))

(with-package (skewer-mode utility)
  (define-key skewer-mode-map (kbd "C-c $")
    (expose #'skewer-bower-load "jquery" "1.9.1")))

(with-package clojure-mode-autoloads
  (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode)))

(with-package nrepl
  (defadvice nrepl-popup-buffer-display (after nrepl-focus-errors activate)
    "Focus the error buffer after errors, like Emacs normally does."
    (select-window (get-buffer-window nrepl-error-buffer)))
  (defadvice nrepl-eval-last-expression (after nrepl-flash-last activate)
    (flash-region (save-excursion (backward-sexp) (point)) (point)))
  (defadvice nrepl-eval-expression-at-point (after nrepl-flash-at activate)
    (apply #'flash-region (nrepl-region-for-expression-at-point)))
  ;; Remove ":headless" to work around Leiningen bug
  (setq nrepl-server-command "lein repl"))

(with-package inf-ruby
  (defadvice inf-ruby-output-filter (after ruby-echo (output) activate)
    (macrolet ((r (regex input) `(replace-regexp-in-string ,regex "" ,input)))
      (let ((echo (r "[ \n\r\t]+$" (r inf-ruby-prompt-pattern output))))
        (when (> (length echo) 0)
          (message "%s" echo)))))
  (defadvice ruby-send-last-sexp (after ruby-flash-last activate)
    (flash-region (save-excursion (ruby-backward-sexp) (point)) (point)))
  (defadvice ruby-send-definition (after ruby-flash-defun activate)
    (save-excursion
      (ruby-end-of-defun)
      (let ((end (point)))
        (ruby-beginning-of-defun)
        (flash-region (point) end)))))

(with-package ps-print
  (setq ps-print-header nil))

(with-package glsl-mode-autoloads
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.fs$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vs$" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))) ; OpenCL

(with-package erc
  (when (eq 0 (string-match "wello" (user-login-name)))
    (setq erc-nick "skeeto")))

(with-package cc-mode
  (setcdr (assq 'c-basic-offset (cdr (assoc "k&r" c-style-alist))) 4)
  (add-to-list 'c-default-style '(c-mode . "k&r")))

(with-package ielm
  (defadvice ielm-eval-input (after ielm-paredit activate)
    "Begin each ielm prompt with a paredit pair.."
    (paredit-open-round)))

(with-package paredit-autoloads
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(with-package* paren
  (show-paren-mode))

(with-package* parenface
  (set-face-foreground 'paren-face "Gray30")
  (add-hook 'clojure-mode-hook
            (paren-face-add-support clojure-font-lock-keywords)))

(with-package* bracket-face
  (add-hook 'scheme-mode-hook (bracket-face scheme-font-lock-keywords-2))
  (add-hook 'lisp-mode-hook (bracket-face lisp-font-lock-keywords-2))
  (add-hook 'emacs-lisp-mode-hook (bracket-face lisp-font-lock-keywords-2))
  (add-hook 'clojure-mode-hook (bracket-face clojure-font-lock-keywords))

(with-package* (ido ido-ubiquitous)
  (setq ido-enable-flex-matching t)
  (setq ido-show-dot-for-dired t) ; Old habits die hard!
  (setq ido-everywhere t)
  (ido-mode 1)
  (ido-ubiquitous-mode)
  (setq ido-ubiquitous-enable-compatibility nil)))

(with-package* smex
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

(with-package* custom
  (load-theme 'wombat t)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (let ((themes custom-enabled-themes))
                (mapc 'disable-theme themes)
                (mapc 'enable-theme (reverse themes)))))
  ;; Fix broken faces between Wombat and Magit
  (custom-set-faces
   '(diff-added           ((t :foreground "green")))
   '(diff-removed         ((t :foreground "red")))
   '(highlight            ((t (:background "black"))))
   '(magit-item-highlight ((t :background "black")))))

(with-package javadoc-lookup-autoloads
  (global-set-key (kbd "C-h j") 'javadoc-lookup))

(with-package javadoc-lookup
  (javadoc-add-artifacts
   [org.lwjgl.lwjgl lwjgl "2.8.2"]
   [com.nullprogram native-guide "0.2"]
   [junit junit "4.10"]
   [org.projectlombok lombok "0.10.4"]
   [org.mockito mockito-all "1.9.0"]
   [com.beust jcommander "1.25"]
   [com.google.guava guava "12.0"]
   [org.jbox2d jbox2d-library "2.1.2.2"]
   [org.apache.commons commons-math3 "3.0"]
   [org.pcollections pcollections "2.1.2"]
   [org.xerial sqlite-jdbc "3.7.2"]
   [com.googlecode.lanterna lanterna "2.1.2"]
   [joda-time joda-time "2.1"]
   [org.apache.lucene lucene-core "3.3.0"]))

(with-package browse-url
  (when (executable-find "firefox")
    (setq browse-url-browser-function 'browse-url-firefox)))

(with-package* yasnippet
  (yas-global-mode 1)
  (yas/load-directory "~/.emacs.d/yasnippet-java")
  (defun disable-yas ()
    (yas-minor-mode -1))
  (add-hook 'emacs-lisp-mode-hook 'disable-yas))

(with-package multiple-cursors-autoloads
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(with-package graphviz-dot-mode
  (setq graphviz-dot-indent-width 2)
  (setq graphviz-dot-auto-indent-on-semi nil))

(with-package* uuid-simple
  (global-set-key "\C-x!" 'uuid-insert)
  (random (make-uuid)))

(with-package* compile-bind
  (compile-bind* (current-global-map)
                 'make ("C-x c" ""
                        "C-x r" 'run
                        "C-x C" 'clean)))

(provide 'init) ; make (require 'init) happy

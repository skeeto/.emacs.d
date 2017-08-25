;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this at the top of your .emacs file for local overrides:
;;     (let ((init "~/.emacs.d/init.elc"))
;;       (if (file-exists-p init)
;;           (load-file init)
;;         (load-file (substring init 0 -1))))

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s" emacs-version))
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s/lisp" emacs-version))
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s/etc" emacs-version))

;; Package bootstrap
(load-file "~/.emacs.d/packages.el")
(require 'autoloads)
(setf package-enable-at-startup nil)
(require 'use-package)

;; "Local" packages
(require 'unannoy)
(require 'extras)
(require 'ctags)

;; Some global keybindings
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "<f5>") (expose #'revert-buffer nil t))
(global-set-key (kbd "C-=") #'calc)
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

;; Frames and fonts

(defvar my-preferred-fonts
  '("Noto Mono-10"
    "Inconsolata-12"))

(defun my-set-preferred-font (&optional frame)
  "Set the first available font from `my-preferred-fonts'."
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font my-preferred-fonts)
        (when (ignore-errors (x-list-fonts font))
          (set-frame-font font)
          (throw 'done nil))))))

(defun my-set-frame-fullscreen (&optional frame)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(add-hook 'after-make-frame-functions #'my-set-preferred-font)
(add-hook 'after-make-frame-functions #'my-set-frame-fullscreen t)

;;; Individual package configurations

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package impatient-mode
  :defer t
  :config
  (defun imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))
  (push (cons 'markdown-mode #'imp-markdown-filter)
        imp-default-user-filters))

(use-package dired
  :defer t
  :config
  (progn
    (add-hook 'dired-mode-hook #'toggle-truncate-lines)
    (setf dired-listing-switches "-alhG"
          dired-guess-shell-alist-user
          '(("\\.pdf\\'" "evince")
            ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
            ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
            ("\\.gif\\'" "animate")))))

(use-package elfeed
  :defer t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (require 'feed-setup)
  (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))

(use-package youtube-dl
  :defer t
  :bind ("C-x y" . youtube-dl-list))

(use-package lisp-mode
  :defer t
  :config
  (progn
    (defun ert-all ()
      (interactive)
      (ert t))
    (defun ielm-repl ()
      (interactive)
      (pop-to-buffer (get-buffer-create "*ielm*"))
      (ielm))
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
    (defalias 'lisp-interaction-mode 'emacs-lisp-mode)
    (font-lock-add-keywords
     'emacs-lisp-mode
     `((,(concat "(\\(\\(?:\\(?:\\sw\\|\\s_\\)+-\\)?"
                 "def\\(?:\\sw\\|\\s_\\)*\\)\\_>"
                 "\\s-*'?" "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
        (1 'font-lock-keyword-face)
        (2 'font-lock-function-name-face nil t)))
     :low-priority)))

(use-package evil
  :init
  (setf evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (defvar my-leader-map
    (let ((map (make-sparse-keymap)))
      (prog1 map
        (define-key map "w" 'elfeed)
        (define-key map "g" 'magit-status))))
  (define-key evil-normal-state-map "\\" my-leader-map)
  (define-key evil-normal-state-map (kbd "M-.") #'ctags-jump)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  (add-to-list 'evil-emacs-state-modes 'youtube-dl-list-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setf evil-ex-search-highlight-all nil)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
  (add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package evil-magit
  :defer t
  :init (require 'evil-magit))

(use-package evil-smartparens
  :defer t
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (sp-use-paredit-bindings))

(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

(use-package comint
  :defer t
  :config
  (progn
    (define-key comint-mode-map (kbd "<down>") #'comint-next-input)
    (define-key comint-mode-map (kbd "<up>") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-n") #'comint-next-input)
    (define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
    (setf comint-prompt-read-only t
          comint-history-isearch t)))

(use-package tramp
  :defer t
  :config
  (setf tramp-persistency-file-name
        (concat temporary-file-directory "tramp-" (user-login-name))))

(use-package whitespace-cleanup-mode
  :config
  (setq-default indent-tabs-mode nil)
  (global-whitespace-cleanup-mode))

(use-package diff-mode
  :defer t
  :config (add-hook 'diff-mode-hook #'read-only-mode))

(use-package color-theme-sanityinc-tomorrow
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night)
  (global-hl-line-mode 1)
  (custom-set-faces
   '(cursor ((t :background "#eebb28")))))

(use-package simple
  :defer t
  :config
  (progn
    ;; disable so I don't use it by accident
    (define-key visual-line-mode-map (kbd "M-q") (expose (lambda ())))
    (add-hook 'tabulated-list-mode-hook #'hl-line-mode)))

(use-package uniquify
  :defer t
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package winner
  :config
  (winner-mode 1)
  (windmove-default-keybindings))

(use-package calc
  :defer t
  :config (setf calc-display-trail nil))

(use-package eshell
  :defer t
  :bind ([f1] . eshell-as)
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell"))
  :config
  (add-hook 'eshell-mode-hook ; Bad, eshell, bad!
            (lambda ()
              (define-key eshell-mode-map (kbd "<f1>") #'quit-window))))

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :init (setf magit-last-seen-setup-instructions "2.1.0")
  :config
  (setf vc-display-status nil
        magit-push-always-verify nil)
  (remove-hook 'git-commit-finish-query-functions
               'git-commit-check-style-conventions))

(use-package gitconfig-mode
  :defer t
  :config (add-hook 'gitconfig-mode-hook
                    (lambda ()
                      (setf indent-tabs-mode nil
                            tab-width 4))))

(use-package markdown-mode
  :defer t
  :mode ("\\.md$" "\\.markdown$" "vimperator-.+\\.tmp$")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'fill-nobreak-predicate
                           'markdown-inside-link-p t)))
  (setf sentence-end-double-space nil
        markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart"))

(use-package octave
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  (setf octave-block-offset 4))

(use-package simple-httpd
  :defer t
  :functions httpd-send-header
  :config
  (progn
    (defservlet uptime "text/plain" ()
      (princ (emacs-uptime)))
    (defun httpd-here ()
      (interactive)
      (setf httpd-root default-directory))
    (defadvice httpd-start (after httpd-query-on-exit-flag activate)
      (let ((httpd-process (get-process "httpd")))
        (when httpd-process
          (set-process-query-on-exit-flag httpd-process nil))))))

(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
  (setf js2-skip-preprocessor-directives t)
  (setq-default js2-additional-externs
                '("$" "unsafeWindow" "localStorage" "jQuery"
                  "setTimeout" "setInterval" "location" "skewer"
                  "console" "phantom")))

(use-package skewer-mode
  :defer t
  :init
  (skewer-setup))

(use-package skewer-repl
  :defer t
  :config (define-key skewer-repl-mode-map (kbd "C-c C-z") #'quit-window))

(use-package ps-print
  :defer t
  :config (setf ps-print-header nil))

(use-package glsl-mode
  :defer t
  :mode ("\\.fs$" "\\.vs$"))

(use-package erc
  :defer t
  :config
  (when (eq 0 (string-match "wello" (user-login-name)))
    (setf erc-nick "skeeto")))

(use-package cc-mode
  :defer t
  :init
  (defun skeeto/c-hook ()
    (setf c-basic-offset 4)
    (c-set-offset 'case-label '+)
    (c-set-offset 'access-label '/)
    (c-set-offset 'label '/))
  :config
  (progn
    (define-key java-mode-map (kbd "C-x I") 'add-java-import)
    (add-hook 'c-mode-hook #'skeeto/c-hook)
    (add-hook 'c++-mode-hook #'skeeto/c-hook)
    (add-to-list 'c-default-style '(c-mode . "k&r"))
    (add-to-list 'c-default-style '(c++-mode . "k&r"))))

(use-package nasm-mode
  :defer t
  :mode ("\\.nasm$" "\\.asm$" "\\.s$")
  :config
  (add-hook 'nasm-mode-hook (lambda () (setf indent-tabs-mode t))))

(use-package asm-mode
  :defer t
  :init
  (add-hook 'asm-mode-hook (lambda () (setf indent-tabs-mode t
                                            tab-always-indent t))))

(use-package x86-lookup
  :defer t
  :bind ("C-h x" . x86-lookup)
  :functions x86-lookup-browse-pdf-evince
  :config
  (let ((pdf-regexp "^64-ia-32-.*-instruction-set-.*\\.pdf$")
        (pdf-dir "~/doc/"))
    (setf x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-evince
          x86-lookup-pdf (ignore-errors
                           (car (directory-files pdf-dir t pdf-regexp))))))

(use-package ielm
  :defer t
  :config
  (define-key ielm-map (kbd "C-c C-z") #'quit-window))

(use-package paren
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
  (setf rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4"))

(use-package icomplete
  :init
  (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<C-tab>" . minibuffer-force-complete)))

(use-package etags
  :defer t
  :config
  (defun etags-build (directory)
    (interactive "DDirectory: ")
    (let* ((results ())
           (head (list directory))
           (tail head))
      (while head
        (dolist (file (directory-files (car head) t nil t))
          (cond ((and (not (string-match "\\.$" file))
                      (not (string-match "\\.\\.$" file))
                      (file-directory-p file))
                 (let ((new-tail (list file)))
                   (setf (cdr tail) new-tail
                         tail new-tail)))
                ((string-match "\\.[ch]$" file)
                 (push file results))))
        (pop head))
      (let ((default-directory directory))
        (apply #'call-process "etags" nil nil nil results)))))

(use-package javadoc-lookup
  :defer t
  :bind ("C-h j" . javadoc-lookup)
  :config
  (ignore-errors
    (setf javadoc-lookup-cache-dir (locate-user-emacs-file "local/javadoc"))))

(use-package gnuplot-mode
  :defer t)

(use-package browse-url
  :defer t
  :init
  (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox
          browse-url-generic-program "xombrero"
          browse-url-generic-args '("-n"))))

(use-package graphviz-dot-mode
  :defer t
  :config
  (setf graphviz-dot-indent-width 2
        graphviz-dot-auto-indent-on-semi nil))

(use-package uuid-simple
  :demand t
  :bind ("C-x !" . uuid-insert)
  :config (random (make-uuid)))

(use-package compile-bind
  :demand t
  :bind (("C-h g" . compile-bind-set-command)
         ("C-h G" . compile-bind-set-root-file))
  :config
  (progn
    (setf compilation-always-kill t
          compilation-scroll-output 'first-error
          compile-bind-command (format "make -kj%d " (numcores)))
    (when (executable-find "nmake.exe")
      (compile-bind-set-command "nmake -nologo "))
    (compile-bind* (current-global-map)
                   ("C-x c" ""
                    "C-x t" 'test
                    "C-x C" 'clean))))

(use-package yaml-mode
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate))))

(use-package jekyll
  :demand t
  :functions httpd-send-header
  :config
  (progn
    (setf jekyll-home "~/src/skeeto.github.com/")
    (when (file-exists-p jekyll-home)
      (require 'simple-httpd)
      (setf httpd-root (concat jekyll-home "_site"))
      (ignore-errors
        (httpd-start)
        (jekyll/start))
      (defservlet robots.txt text/plain ()
        (insert "User-agent: *\nDisallow: /\n")))))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package gamegrid
  :defer t
  :init
  (setf gamegrid-user-score-file-directory (locate-user-emacs-file "games")))

(use-package pov-mode
  :defer t
  :init
  (autoload 'irfc-mode "irfc" nil t)
  (autoload 'irfc-visit "irfc" nil t)
  (setf irfc-directory (locate-user-emacs-file "local/rfc")
        irfc-assoc-mode t)
  (mkdir irfc-directory t))

(use-package ospl-mode
  :defer t
  :init
  (autoload 'ospl-mode "ospl-mode"))

(use-package sql
  :defer t
  :init
  (setf sql-product 'sqlite))

(use-package enriched
  :defer t
  :config
  (define-key enriched-mode-map "\C-m" nil))

(provide 'init) ; make (require 'init) happy

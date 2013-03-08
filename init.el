;; Set up load-path for everything
(add-to-list 'load-path "~/.emacs.d")
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Set up package system
(defvar my-packages
  '(clojure-mode glsl-mode graphviz-dot-mode ido-ubiquitous
    impatient-mode javadoc-lookup js2-mode lua-mode magit markdown-mode
    memoize multiple-cursors nrepl paredit parenface rdp simple-httpd
    skewer-mode smex yasnippet)
  "A list of packages to ensure are installed at launch.")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'cl)
(require 'memoize)
(require 'imgur)
(require 'my-funcs) ; custom functions
(require 'utility)

;; Seed the PRNG
(random t)

;; Custom general bindings
(global-set-key (kbd "C-S-j") 'join-line)

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
(defalias 'yes-or-no-p 'y-or-n-p)
(setq dabbrev-case-distinction nil)
(setq dabbrev-case-fold-search nil)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)
(mapatoms (lambda (sym)
            (if (get sym 'disabled)
                (put sym 'disabled nil))))

;; Emacs Lisp
(defalias 'lisp-interaction-mode 'emacs-lisp-mode)

(defun ielm-repl ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*ielm*"))
  (ielm))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm-repl)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)

;; Display the time
(setq display-time-default-load-average nil)
(setq display-time-use-mail-icon t)
(setq display-time-24hr-format t)
(display-time-mode t)

;; Fix up comint annoyances
(eval-after-load 'comint
  '(progn
     (message "comint loaded: %s" (featurep 'comint))
     (setq comint-prompt-read-only t
           comint-history-isearch t)
     (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
     (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
     (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
     (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
     (define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward)))

;; tramp
(eval-after-load 'tramp-cache
  '(setq tramp-persistency-file-name
         (concat temporary-file-directory "tramp-" (user-login-name))))

;; Use proper whitespace
(require 'whitespace)
(setq-default indent-tabs-mode nil)
(defcustom do-whitespace-cleanup t "Perform whitespace-cleanup on save.")
(make-variable-buffer-local 'do-whitespace-cleanup)
(defun toggle-whitespace-cleanup ()
  "Turn the whitespace-cleanup hook on and off."
  (interactive)
  (setq do-whitespace-cleanup (not do-whitespace-cleanup))
  (message "do-whitespace-cleanup set to %s" do-whitespace-cleanup))
(add-hook 'before-save-hook
          (lambda ()
            (when (and (not buffer-read-only) do-whitespace-cleanup)
              ;; turn off and on to work around Emacs bug #4069
              (whitespace-turn-on)
              (whitespace-turn-off)
              (whitespace-cleanup))))
(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

;; visual-line-mode
(eval-after-load 'simple
  '(define-key visual-line-mode-map (kbd "M-q")
     (lambda () (interactive)))) ; disable so I don't use it by accident

;; Uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Winner mode
(require 'winner)
(winner-mode 1)
(windmove-default-keybindings)

;; org-mode
(setq org-replace-disputed-keys t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)

;; Git
(global-set-key "\C-xg" 'magit-status)
(setq vc-display-status nil)

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("pentadactyl.txt$" . markdown-mode))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "<tab>") nil)) ; fix for YASnippet

;; Jekyll
(require 'jekyll)
(require 'simple-httpd)
(setq httpd-servlets t)
(setq jekyll-home "~/src/skeeto.github.com/")
(when (file-exists-p jekyll-home)
  (setq httpd-root (concat jekyll-home "_site"))
  (condition-case error
      (progn
        (httpd-start)
        (jekyll/start))
    (error nil)))
(defservlet robots.txt text/plain ()
  (insert "User-agent: *\nDisallow: /\n"))

;;; JavaScript
(eval-after-load 'js2-mode
  '(progn
     (add-hook 'js2-mode-hook (lambda () (setq mode-name "js2")))
     (setq js2-skip-preprocessor-directives t)
     (setq-default js2-additional-externs
                   '("$" "unsafeWindow" "localStorage" "jQuery"
                     "setTimeout" "setInterval" "location" "skewer"
                     "console" "phantom"))))

;; Clojure
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

; Fix nrepl.el's broken error buffer
(eval-after-load 'nrepl
  '(defalias 'nrepl-popup-buffer-quit 'quit-window))
(defadvice nrepl-popup-buffer-display (after nrepl-focus-errors activate)
  "Focus the error buffer after errors, like Emacs normally does."
  (select-window (get-buffer-window nrepl-error-buffer)))

(defadvice nrepl-eval-last-expression (after nrepl-flash-last activate)
  (flash-region (save-excursion (backward-sexp) (point)) (point)))

(defadvice nrepl-eval-expression-at-point (after nrepl-flash-at activate)
  (apply #'flash-region (nrepl-region-for-expression-at-point)))

; Remove ":headless" to work around Leiningen bug
(setq nrepl-server-command "lein repl")

;; Octave
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Printing
(eval-after-load 'ps-print
  '(setq ps-print-header nil))

;; GLSL
(add-to-list 'auto-mode-alist '("\\.glsl$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.fs$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs$" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode)) ; OpenCL

;; groff
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))

;; ERC (only set it for me)
(if (eq 0 (string-match "wello" (user-login-name)))
    (setq erc-nick "skeeto"))

;; C (and fix Emacs' incorrect k&r indentation)
(eval-after-load 'cc-mode
  '(progn
     (setcdr (assq 'c-basic-offset (cdr (assoc "k&r" c-style-alist))) 4)
     (add-to-list 'c-default-style '(c-mode . "k&r"))))

;; Parenthesis
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each ielm prompt with a paredit pair.."
  (paredit-open-round))
(show-paren-mode)
(require 'parenface)
(set-face-foreground 'paren-face "Gray30")
(add-hook 'clojure-mode-hook
          (paren-face-add-support clojure-font-lock-keywords))

;; Brackets
(defvar bracket-face 'bracket-face)
(defface bracket-face
    '((((class color))
       (:foreground "Gray60")))
  "Face for displaying a bracket."
  :group 'faces)

(defmacro bracket-face (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
     (let* ((regexp"\\[\\|\\]\\|{\\|}")
            (match (assoc regexp ,keywords)))
       (unless (eq (cdr match) bracket-face)
         (setq ,keywords (cons (cons regexp bracket-face) ,keywords))))))

(add-hook 'scheme-mode-hook (bracket-face scheme-font-lock-keywords-2))
(add-hook 'lisp-mode-hook (bracket-face lisp-font-lock-keywords-2))
(add-hook 'emacs-lisp-mode-hook (bracket-face lisp-font-lock-keywords-2))
(add-hook 'clojure-mode-hook (bracket-face clojure-font-lock-keywords))

;; ERT
(defun ert-silently ()
  (interactive)
  (ert t))
(define-key emacs-lisp-mode-map (kbd "C-x r") 'ert-silently)

;; Ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t) ; Old habits die hard!
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode)
(setq ido-ubiquitous-enable-compatibility nil)

;; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Set the color theme
(load-theme 'wombat t)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((themes custom-enabled-themes))
              (mapc 'disable-theme themes)
              (mapc 'enable-theme (reverse themes)))))
;(set-face-attribute 'default nil :height 100)
;(set-frame-parameter (selected-frame) 'alpha 80)
;(set-default-font "Inconsolata-12")

;; Java
(global-set-key (kbd "C-h j") 'javadoc-lookup)
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
 [org.apache.lucene lucene-core "3.3.0"])

;; Browser
(when (executable-find "firefox")
  (setq browse-url-browser-function 'browse-url-firefox))

;; YASnippet
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/yasnippet-java")
(defun disable-yas ()
  (yas-minor-mode -1))
(add-hook 'emacs-lisp-mode-hook 'disable-yas)

;; Scheme
(eval-after-load 'geiser
  '(font-lock-add-keywords 'scheme-mode
                           '(("define-\\w+" . font-lock-keyword-face))))

;; mark-multiple
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Custom bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-k" 'compile)
(global-set-key [f1] (lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key [f2] (expose (apply-partially 'revert-buffer nil t)))
(global-set-key [f5] (lambda () (interactive) (mapatoms 'byte-compile)))

;; graphviz-dot-mode
(eval-after-load 'graphviz-dot-mode
  '(progn
     (setq graphviz-dot-indent-width 2)
     (setq graphviz-dot-auto-indent-on-semi nil)))

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

;; Fix broken faces from Wombat and Magit
(custom-set-faces
 '(diff-added           ((t :foreground "green")))
 '(diff-removed         ((t :foreground "red")))
 '(highlight            ((t (:background "black"))))
 '(magit-item-highlight ((t :background "black"))))

(provide 'init) ; for those who want to (require 'init)

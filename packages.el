(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "2.3")
 ("elfeed" "https://github.com/skeeto/elfeed"
  "2.1.1"
  :removal '("^web$"))
 ("evil" "https://github.com/emacs-evil/evil"
  "1.2.9"
  :removal '("evil-tests.el" "^ert.el"))
 ("evil-magit" "https://github.com/emacs-evil/evil-magit"
  "v0.4.1")
 ("evil-cleverparens" "https://github.com/luxbock/evil-cleverparens"
  "82c920b")
 ("sanityinc" "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  "1.17")
 ("simple-httpd" "https://github.com/skeeto/emacs-web-server"
  "1.4.6")
 ("htmlize" "https://github.com/hniksic/emacs-htmlize"
  "release/1.51")
 ("impatient-mode" "https://github.com/skeeto/impatient-mode" "1.0.0")
 ("whitespace-cleanup-mode"
  "https://github.com/purcell/whitespace-cleanup-mode" "0.2")
 ("async" "https://github.com/jwiegley/emacs-async"
  "v1.9.2")
 ("dash" "https://github.com/magnars/dash.el"
  "2.13.0"
  :removal '("^dev$"))
 ("with-editor" "https://github.com/magit/with-editor"
  "v2.5.11")
 ("magit" "https://github.com/magit/magit"
  "2.10.3")
 ("git-modes" "https://github.com/magit/git-modes"
  "1.2.4")
 ("paredit" "http://mumble.net/~campbell/git/paredit.git/"
  "v24")
 ("smartparens" "https://github.com/Fuco1/smartparens"
  "1.10.1")
 ("markdown-mode" "https://github.com/jrblevin/markdown-mode"
  "v2.2")
 ("js2-mode" "https://github.com/mooz/js2-mode"
  "20170116")
 ("skewer-mode" "https://github.com/skeeto/skewer-mode"
  "18a90f4")
 ("glsl-mode" "https://github.com/jimhourihan/glsl-mode"
  "b470964")
 ("nasm-mode" "https://github.com/skeeto/nasm-mode"
  "1.1.1")
 ("x86-lookup" "https://github.com/skeeto/x86-lookup"
  "1.1.1")
 ("rainbow-delimiters" "https://github.com/Fanael/rainbow-delimiters"
  "2.1.3")
 ("javadoc-lookup" "https://github.com/skeeto/javadoc-lookup"
  "1.1.0")
 ("gnuplot-mode" "https://github.com/mkmcc/gnuplot-mode"
  "296ff8d")
 ("graphviz-dot-mode" "https://github.com/ppareit/graphviz-dot-mode"
  "v0.4")
 ("yaml-mode" "https://github.com/yoshiki/yaml-mode"
  "v0.0.13")
 ("pov-mode" "https://github.com/melmothx/pov-mode"
  "9fc1db3"))

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-root "evil" "lib"))
(add-to-list 'load-path (gpkg-root "magit" "lisp"))

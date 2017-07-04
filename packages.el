(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "cd58b268a8a025451c11c3cb1ba18d4f27f245da") ; 2.3
 ("elfeed" "https://github.com/skeeto/elfeed"
  "67c4f07f5b0f942f030848f5d657909a1424b597"  ; 2.1.1
  :removal '("^web$"))
 ("evil" "https://github.com/emacs-evil/evil"
  "cecbcfbdda16653e3b5ddc7de2eab25537ec154b"  ; 1.2.13-ish
  :removal '("evil-tests.el" "^ert.el"))
 ("evil-magit" "https://github.com/emacs-evil/evil-magit"
  "077354f8ebd5da76937bf8f5df5d484f8a0ccc62") ; v0.4.1
 ("evil-cleverparens" "https://github.com/luxbock/evil-cleverparens"
  "82c920ba04accfd31fa292e11c454d5112b4fd51") ; 2016-06-11T23:04:45+07:00
 ("sanityinc" "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  "81d8990085960824f700520d08027e6aca58feaa") ; 1.17
 ("simple-httpd" "https://github.com/skeeto/emacs-web-server"
  "b191b07c942e44c946a22a826c4d9c9a0475fd7e") ; 1.4.6
 ("htmlize" "https://github.com/hniksic/emacs-htmlize"
  "88e2cb6588827893d7bc619529393887c264d15a") ; release/1.51
 ("impatient-mode" "https://github.com/skeeto/impatient-mode"
  "eba1efce3dd20b5f5017ab64bae0cfb3b181c2b0") ; 1.0.0
 ("whitespace-cleanup-mode" "https://github.com/purcell/whitespace-cleanup-mode"
  "0cb638474c206f342df0f9652f232f17804b478e") ; 0.2
 ("async" "https://github.com/jwiegley/emacs-async"
  "d6222c2959caaca5c4980038756097da8a51e505") ; v1.9.2
 ("dash" "https://github.com/magnars/dash.el"
  "4ae329aa2160411c8b47794de067fcf29bc38a22"  ; 2.13.0
  :removal '("^dev$"))
 ("with-editor" "https://github.com/magit/with-editor"
  "3385ffdc6faed5a283e26a7ebf89825c700dd395") ; v2.5.11
 ("magit" "https://github.com/magit/magit"
  "d5b747473ab0bf0f0034fca2f57f6497d476b67e") ; 2.10.3
 ("git-modes" "https://github.com/magit/git-modes"
  "af4ff3222f38daa0d352afdf3d20741b4fab2e79") ; 1.2.4
 ("paredit" "http://mumble.net/~campbell/git/paredit.git/"
  "82bb75ceb2ddc272d6618d94874b7fc13181a409") ; v24
 ("smartparens" "https://github.com/Fuco1/smartparens"
  "7841b2f02a1a99e1cb166d595f24f16a514ccbb5") ; 1.10.1
 ("markdown-mode" "https://github.com/jrblevin/markdown-mode"
  "e9bb47d8d87ae8205ed935a3b485e12c12e43aa9") ; v2.2
 ("js2-mode" "https://github.com/mooz/js2-mode"
  "03c679eb9914d58d7d9b7afc2036c482a9a01236") ; 20170116
 ("skewer-mode" "https://github.com/skeeto/skewer-mode"
  "18a90f401451f8ca0486bdaf45647ac3ccebc0ac") ; 1.6.2-10-g18a90f4
 ("glsl-mode" "https://github.com/jimhourihan/glsl-mode"
  "b4709644bb01b522ba9c8afe16ed2394783c481f") ; 2016-02-09T08:33:51-08:00
 ("nasm-mode" "https://github.com/skeeto/nasm-mode"
  "d990ed94d902b74a5c834fb567e03307607cee45") ; 1.1.1
 ("x86-lookup" "https://github.com/skeeto/x86-lookup"
  "208810ea93214491e6e2329cdbf81de85437939a") ; 1.1.1
 ("rainbow-delimiters" "https://github.com/Fanael/rainbow-delimiters"
  "93cd2dc873e7fedca7abc599cd97d46db4376ac7") ; 2.1.3
 ("javadoc-lookup" "https://github.com/skeeto/javadoc-lookup"
  "507a2dd443d60b537b8f779c1847e2cd0ccd1382") ; 1.1.0
 ("gnuplot-mode" "https://github.com/mkmcc/gnuplot-mode"
  "296ff8d263513cdfb8e85b06e2441c751565b793") ; 2015-11-22T23:39:24-08:00
 ("graphviz-dot-mode" "https://github.com/ppareit/graphviz-dot-mode"
  "7301cc276206b6995d265bcb9eb308bb83c760be") ; v0.4
 ("yaml-mode" "https://github.com/yoshiki/yaml-mode"
  "2ace378bef2047a980fba0e42e3e6b5d990f2c66") ; v0.0.13
 ("pov-mode" "https://github.com/melmothx/pov-mode"
  "9fc1db3aab7c27155674dd1a87ec62606035d074")); 2016-11-15T08:43:14+01:00

;; Magit annoyance workaround
(setf magit-version (gpkg-package-ref "magit"))

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "evil" "lib"))
(add-to-list 'load-path (gpkg-path "magit" "lisp"))

(require 'gpkg)

(gpkg-config
 ("use-package" "https://github.com/jwiegley/use-package"
  "cd58b268a8a025451c11c3cb1ba18d4f27f245da") ; 2.3
 ("elfeed" "https://github.com/skeeto/elfeed"
  "00b25d974abc4f3e61676068397758035bfdfc30"  ; 2.3.0
  :removal '("^web$"))
 ("youtube-dl" "https://github.com/skeeto/youtube-dl-emacs"
  "43758426ddf6bbe87f5a837cd0c6f8e393e003b6") ; 1.0
 ("evil" "https://github.com/emacs-evil/evil"
  "427cf5faa57e8794ac93f594dc3d1972e687a25a"  ; 1.2.13
  :removal '("evil-tests.el" "^ert.el"))
 ("evil-smartparens" "https://github.com/expez/evil-smartparens"
  "9fe4eed1c6327197afe6c13bb0771e18908aff00"  ; 0.4.0
  :removal '("^tests$"))
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
 ("dash" "https://github.com/magnars/dash.el"
  "4ae329aa2160411c8b47794de067fcf29bc38a22"  ; 2.13.0
  :removal '("^dev$"))
 ("smartparens" "https://github.com/Fuco1/smartparens"
  "4873352b5d0a1c5142658122de1b6950b8fe7e4d") ; 1.11.0
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
 ("pov-mode" "https://github.com/melmothx/pov-mode"
  "9fc1db3aab7c27155674dd1a87ec62606035d074")); 2016-11-15T08:43:14+01:00

;; Set up some extra load-path directories
(add-to-list 'load-path (gpkg-path "evil" "lib"))

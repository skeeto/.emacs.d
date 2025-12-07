;;; feed-setup.el --- customize my web feeds

(require 'cl-lib)
(require 'elfeed)
(require 'youtube-dl)

(setq-default elfeed-search-filter "@1-week-ago +unread")

;; More keybindings

(define-key elfeed-search-mode-map "h"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter (default-value 'elfeed-search-filter))))

(define-key elfeed-search-mode-map (kbd "j") #'next-line)
(define-key elfeed-search-mode-map (kbd "k") #'previous-line)

(define-key elfeed-search-mode-map (kbd "l")
  (lambda ()
    (interactive)
    (switch-to-buffer (elfeed-log-buffer))))

(define-key elfeed-search-mode-map "t"
  (lambda ()
    (interactive)
    (cl-macrolet ((re (re rep str) `(replace-regexp-in-string ,re ,rep ,str)))
      (elfeed-search-set-filter
       (cond
        ((string-match-p "-youtube" elfeed-search-filter)
         (re " *-youtube" " +youtube" elfeed-search-filter))
        ((string-match-p "\\+youtube" elfeed-search-filter)
         (re " *\\+youtube" " -youtube" elfeed-search-filter))
        ((concat elfeed-search-filter " -youtube")))))))

(setf elfeed-search-clipboard-type 'CLIPBOARD)

(defun elfeed-podcast-yank ()
  "Clean up and copy the first enclosure URL into the clipboard."
  (interactive)
  (let* ((entry (elfeed-search-selected t))
         (url (caar (elfeed-entry-enclosures entry)))
         (fixed (replace-regexp-in-string "\\?.*$" "" url)))
    (if (fboundp 'gui-set-selection)
        (gui-set-selection elfeed-search-clipboard-type fixed)
      (with-no-warnings
        (x-set-selection elfeed-search-clipboard-type fixed)))
    (elfeed-untag entry 'unread)
    (message "Copied: %s" fixed)
    (unless (use-region-p) (forward-line))))

(define-key elfeed-search-mode-map "Y" #'elfeed-podcast-yank)

;; youtube-dl config

(setq youtube-dl-directory "~/netshare"
      youtube-dl-arguments '("--no-mtime" "-t" "mp4" "--no-colors"))

(defface elfeed-youtube
  '((t :foreground "#f9f"))
  "Marks YouTube videos in Elfeed."
  :group 'elfeed)

(push '(youtube elfeed-youtube)
      elfeed-search-face-alist)

(defun elfeed-show-youtube-dl ()
  "Download the current entry with youtube-dl."
  (interactive)
  (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))

(cl-defun elfeed-search-youtube-dl (&key slow)
  "Download the current entry with youtube-dl."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (if (null (youtube-dl (elfeed-entry-link entry)
                            :title (elfeed-entry-title entry)
                            :slow slow))
          (message "Entry is not a YouTube link!")
        (message "Downloading %s" (elfeed-entry-title entry)))
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless (use-region-p) (forward-line)))))

(defalias 'elfeed-search-youtube-dl-slow
  (expose #'elfeed-search-youtube-dl :slow t))

(define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
(define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
(define-key elfeed-search-mode-map "D" 'elfeed-search-youtube-dl-slow)
(define-key elfeed-search-mode-map "L" 'youtube-dl-list)

;; Special filters

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "7 days ago"
                              :remove 'unread))

(defun tagize-for-elfeed (string)
  "Try to turn STRING into a reasonable Elfeed tag."
  (when (and (< (length string) 24)
             (string-match-p "^[/#]?[[:space:][:alnum:]]+$" string))
    (let* ((down (downcase string))
           (dashed (replace-regexp-in-string "[[:space:]]+" "-" down))
           (truncated (replace-regexp-in-string "^[/#]" "" dashed)))
      (intern truncated))))

(defun add-entry-categories-to-tags (entry)
  (dolist (category (elfeed-meta entry :categories) entry)
    (let ((tag (tagize-for-elfeed category)))
      (when tag
        (elfeed-tag entry tag)))))

(add-hook 'elfeed-new-entry-hook #'add-entry-categories-to-tags)

;; Helpers

(cl-defun elfeed-dead-feeds (&optional (years 1.0))
  "Return a list of feeds that haven't posted en entry in YEARS years."
  (let* ((living-feeds (make-hash-table :test 'equal))
         (seconds (* years 365.0 24 60 60))
         (threshold (- (float-time) seconds)))
    (with-elfeed-db-visit (entry feed)
      (let ((date (elfeed-entry-date entry)))
        (when (> date threshold)
          (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
    (cl-loop for url in (elfeed-feed-list)
             unless (gethash url living-feeds)
             collect url)))

;; Custom faces

(defface elfeed-comic
  '((t :foreground "#BFF"))
  "Marks comics in Elfeed."
  :group 'elfeed)

(push '(comic elfeed-comic)
      elfeed-search-face-alist)

(defface elfeed-audio
  '((t :foreground "#FA0"))
  "Marks podcasts in Elfeed."
  :group 'elfeed)

(push '(audio elfeed-audio)
      elfeed-search-face-alist)

(defface elfeed-important
  '((t :foreground "#E33"))
  "Marks important entries in Elfeed."
  :group 'elfeed)

(push '(important elfeed-important)
      elfeed-search-face-alist)

;; The actual feeds listing

(defvar youtube-feed-format
  '(("^UC"   . "https://www.youtube.com/feeds/videos.xml?channel_id=%s")
    ("^UULF" . "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
    ("^PL"   . "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
    (""      . "https://www.youtube.com/feeds/videos.xml?user=%s")))

(defun elfeed--expand (listing)
  "Expand feed URLs depending on their tags."
  (cl-destructuring-bind (url . tags) listing
    (cond
     ((member 'youtube tags)
      (let* ((case-fold-search nil)
             (test (lambda (s r) (string-match-p r s)))
             (format (cl-assoc url youtube-feed-format :test test)))
        (cons (format (cdr format) url) tags)))
     (listing))))

(defmacro elfeed-config (&rest feeds)
  "Minimizes feed listing indentation without being weird about it."
  (declare (indent 0))
  `(setf elfeed-feeds (mapcar #'elfeed--expand ',feeds)))

(elfeed-config
  ("https://acoup.blog/feed/" blog)
  ("https://blog.cryptographyengineering.com/feed/" blog)
  ("https://astralcodexten.substack.com/feed/" blog philosophy)
  ("https://betonit.substack.com/feed/" blog economics)
  ("https://simblob.blogspot.com/feeds/posts/default" blog dev)
  ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
  ("https://lemire.me/blog/feed/" dev blog)
  ("https://danluu.com/atom.xml" dev blog)
  ("https://www.debian.org/security/dsa" debian list security important)
  ("https://www.debian.org/News/news" debian list)
  ("https://www.filfre.net/feed/" blog history essay)
  ("https://danwang.co/feed/" blog philosophy)
  ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
  ("https://floooh.github.io/feed.xml" blog dev)
  ("https://peter0x44.github.io/index.xml" blog dev)
  ("https://www.exocomics.com/feed" comic)
  ("https://fabiensanglard.net/rss.xml" blog dev)
  ("https://flak.tedunangst.com/rss" dev blog)
  ("https://gcc.gnu.org/git/?p=gcc-wwwdocs.git;a=atom;f=htdocs/releases.html" dev release)
  ("https://git.gnupg.org/cgi-bin/gitweb.cgi?p=gnupg-doc.git;a=rss;f=web/index.org" dev product)
  ("https://github.com/rmyorston/busybox-w32/releases.atom" release product)
  ("https://backend.deviantart.com/rss.xml?q=by%3AGydw1n" image)
  ("https://photo.nullprogram.com/feed/" photo myself)
  ("https://loadingartist.com/feed/" comic)
  ("https://marc-b-reynolds.github.io/feed.xml" dev blog math)
  ("http://www.mazelog.com/rss" math puzzle)
  ("https://sourceforge.net/projects/mingw-w64/rss?path=/mingw-w64/mingw-w64-release" dev release)
  ("https://www.mrmoneymustache.com/feed/" blog philosophy)
  ("https://nrk.neocities.org/rss.xml" blog dev)
  ("https://nullprogram.com/feed/" blog dev myself)
  ("https://blogs.msdn.microsoft.com/oldnewthing/feed" blog dev)
  ("https://www.overcomingbias.com/feed" blog philosophy)
  ("http://feeds.feedburner.com/PoorlyDrawnLines" comic)
  ("https://maskray.me/blog/atom.xml" blog dev)
  ("https://www.npr.org/rss/podcast.php?id=510289" podcast audio economics)
  ("https://possiblywrong.wordpress.com/feed/" blog math puzzle)
  ("http://feeds.wnyc.org/radiolab" audio)
  ("https://www.smbc-comics.com/comic/rss" comic)
  ("https://blog.plover.com/index.atom" blog dev)
  ("https://xkcd.com/atom.xml" comic)
  ("http://hnapp.com/rss?q=host:nullprogram.com" hackernews myself)
  ("https://old.reddit.com/domain/nullprogram.com.rss" reddit myself)
  ("https://old.reddit.com/r/C_Programming/.rss?limit=100" subreddit)
  ("UULFHnyfMqiRRG1u-2MsSQLbXA" youtube) ; Veritasium
  ("adric22" youtube) ; The 8-Bit Guy
  ("craig1black" youtube)              ; Adrian's Digital Basement
  ("UCbtwi4wK1YXd9AyV_4UcE6g" youtube) ; Adrian's Digital Basement ][
  ("UCd8v3SbzGP9_wuSOr_xk_eA" youtube) ; Antique Furniture Restoration
  ("UCH_7doiCkWeq0v3ycWE5lDw" youtube) ; Any Austin
  ("UCbGGg1xyVana3IY4WInzgyg" youtube) ; Blow Fan
  ("damo2986" youtube)
  ("destinws2" youtube)
  ("EEVblog" youtube)
  ("eevblog2" youtube)
  ("foodwishes" youtube)
  ("UULFtWCNdtCS-SG2gKYaYhE7BA" youtube) ; Gaming Jay
  ("UULFN9UPjA8I-uwvAy0-N9maOA" youtube) ; The Generalist Papers
  ("UCuCkxoKLYO_EQ2GeFtbM_bw" youtube) ; Half as Interesting
  ("UCm9K6rby98W8JigLoZOh6FQ" youtube) ; LockPickingLawyer
  ("jastownsendandson" youtube)
  ("MatthiasWandel" youtube)
  ("UC3_AWXcf2K3l9ILVuQe-XwQ" youtube) ; Matthias random stuff
  ("Nerdwriter1" youtube)
  ("numberphile" youtube)
  ("UCNyGbxoEo6CQvaRVEvItxkA" youtube) ; Pask Makes
  ("UULFF1fG3gT44nGTPU2sVLoFWg" youtube) ; Patrick (H) Willems
  ("Pixelmusement" youtube)
  ("PlumpHelmetPunk" youtube)
  ("ProZD" youtube)
  ("XboxAhoy" youtube)
  ("UULFifMPWjBEu7rYx3CVdkjiWA" youtube) ; Jomboy & Jake TV
  ("RedLetterMedia" youtube)
  ("Cercopithecan" youtube) ; Sebastian Lague
  ("UC1_uAIS3r8Vu6JjXWvastJg" youtube) ; Mathologer
  ("standupmaths" youtube)
  ("UCg-_lYeV8hBnDSay7nmphUA" youtube) ; Tally Ho
  ("UCy0tKL1T7wFoYcxCe0xjN6Q" youtube) ; Technology Connections
  ("UClRwC5Vc8HrB6vGx6Ti-lhA" youtube) ; Technology Connextras
  ("UCqrrxZeeFSNCjGmD-33SKMw" youtube) ; u m a m i
  ("handmadeheroarchive" youtube dev)
  ("UCwRqWnW5ZkVaP_lZF7caZ-g" youtube) ; Retro Game Mechanics Explained
  ("phreakindee" youtube)
  ("UCCj_mkYyeGIb9MPSdb74ykA" youtube) ; GET OFF MY LAWN
  ("szyzyg" youtube)
  ("UCsXVk37bltHxD1rDPwtNM8Q" youtube) ; Kurzgesagt – In a Nutshell
  ("UULFtKUW8LJK2Ev8hUy9ZG_PPA" youtube) ; Welker Farms
  ("Wendoverproductions" youtube))

(provide 'feed-setup)

;;; feed-setup.el ends here

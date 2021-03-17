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
      youtube-dl-arguments
      (nconc `("-f" "bestvideo[height<=1080]+bestaudio/best[height<=1080]")
             youtube-dl-arguments))

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
  '(("^UC" . "https://www.youtube.com/feeds/videos.xml?channel_id=%s")
    ("^PL" . "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
    (""    . "https://www.youtube.com/feeds/videos.xml?user=%s")))

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
  ("http://esr.ibiblio.org/?feed=rss2" blog)
  ("https://arp242.net/feed.xml" blog dev)
  ("https://blog.cryptographyengineering.com/feed/" blog)
  ("https://www.npr.org/rss/podcast.php?id=510299" audio)
  ("https://astralcodexten.substack.com/feed/" blog philosophy)
  ("https://begriffs.com/atom.xml" blog dev)
  ("http://english.bouletcorp.com/feed/" comic)
  ("http://bit-player.org/feed" blog math)
  ("https://simblob.blogspot.com/feeds/posts/default" blog dev)
  ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
  ("https://www.buttersafe.com/feed/" comic)
  ("https://lemire.me/blog/feed/" dev blog)
  ("https://danluu.com/atom.xml" dev blog)
  ("https://www.blogger.com/feeds/19727420/posts/default" blog)
  ("https://www.debian.org/security/dsa" debian list security important)
  ("https://www.debian.org/News/news" debian list)
  ("https://dendibakh.github.io/feed.xml" blog dev)
  ("https://www.filfre.net/feed/" blog history essay)
  ("https://drewdevault.com/feed.xml" blog dev)
  ("https://bay12games.com/dwarves/dev_now.rss" blog gaming product)
  ("https://danwang.co/feed/" blog philosophy)
  ("https://www.econlib.org/feed/indexCaplan_xml" blog economics)
  ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
  ("https://floooh.github.io/feed.xml" blog dev)
  ("https://feeds.megaphone.fm/everylittlething" audio)
  ("http://explosm.net/rss" comic)
  ("https://www.exocomics.com/feed" comic)
  ("https://fabiensanglard.net/rss.xml" blog dev)
  ("https://freakonomics.com/feed/" audio)
  ("https://flak.tedunangst.com/rss" dev blog)
  ("https://geoff.greer.fm/feed.atom" blog dev)
  ("https://gcc.gnu.org/git/?p=gcc-wwwdocs.git;a=atom;f=htdocs/releases.html" dev release)
  ("https://groups.google.com/forum/feed/golang-announce/topics/rss.xml?num=50" dev release)
  ("https://git.gnupg.org/cgi-bin/gitweb.cgi?p=gnupg-doc.git;a=rss;f=web/index.org" dev product)
  ("https://github.com/rmyorston/busybox-w32/releases.atom" release product)
  ("https://goodbearcomics.com/feed/" comic)
  ("https://backend.deviantart.com/rss.xml?q=by%3AGydw1n" image)
  ("https://www.hackerfactor.com/blog/rss.php?version=2.0" dev blog)
  ("https://irreal.org/blog/?feed=rss2" blog)
  ("https://www.joelonsoftware.com/feed/" blog dev)
  ("https://jobtalle.com/rss.xml" blog dev)
  ("https://photo.nullprogram.com/feed/" photo myself)
  ("https://loadingartist.com/feed/" comic)
  ("https://marc-b-reynolds.github.io/feed.xml" dev blog math)
  ("http://www.mazelog.com/rss" math puzzle)
  ("https://sourceforge.net/projects/mingw-w64/rss?path=/mingw-w64/mingw-w64-release" dev release)
  ("https://www.mrlovenstein.com/rss.xml" comic)
  ("https://mortoray.com/feed/" blog dev)
  ("https://www.mrmoneymustache.com/feed/" blog philosophy)
  ("https://www.natewoodward.org/blog/atom.xml" blog def)
  ("https://nickdesaulniers.github.io/atom.xml" blog dev)
  ("https://nullprogram.com/feed/" blog dev myself)
  ("https://blogs.msdn.microsoft.com/oldnewthing/feed" blog dev)
  ("https://www.overcomingbias.com/feed" blog philosophy)
  ("https://www.npr.org/rss/podcast.php?id=510289" podcast audio economics)
  ("https://possiblywrong.wordpress.com/feed/" blog math puzzle)
  ("https://priioajn.wordpress.com/feed/" blog esperanto)
  ("https://rachelbythebay.com/w/atom.xml" blog dev)
  ("http://feeds.wnyc.org/radiolab" audio)
  ("https://randomascii.wordpress.com/feed/" blog dev)
  ("https://feeds.megaphone.fm/revisionisthistory" audio)
  ("https://sachachua.com/blog/category/emacs/feed/" emacs blog)
  ("https://www.smbc-comics.com/comic/rss" comic)
  ("https://old.reddit.com/user/softwaring/submitted/.rss" image reddit)
  ("https://stuartwheaton.com/feed.xml" blog dev)
  ("https://feeds.megaphone.fm/stuffyoushouldknow" audio)
  ("https://www.swordscomic.com/swords/feed/" comic)
  ("https://teokajlibroj.wordpress.com/feed/" blog esperanto)
  ("https://theycantalk.com/rss" comic)
  ("https://blog.plover.com/index.atom" blog dev)
  ("https://slatestarcodex.com/feed/" blog philosophy)
  ("https://www.kohney.com/feed/" comic)
  ("https://www.shamusyoung.com/twentysidedtale/?feed=rss2" blog gaming)
  ("http://feeds.wnyc.org/TheAnthropoceneReviewed" audio)
  ("https://www.whompcomic.com/comic/rss" comic)
  ("https://xkcd.com/atom.xml" comic)
  ("http://hnapp.com/rss?q=host:nullprogram.com" hackernews myself)
  ("https://old.reddit.com/domain/nullprogram.com.rss" reddit myself)
  ("https://old.reddit.com/r/dailyprogrammer/.rss" subreddit)
  ("1veritasium" youtube)
  ("UCYO_jab_esuFRV4b17AJtAw" youtube) ; 3Blue1Brown
  ("adric22" youtube) ; The 8-Bit Guy
  ("UCcTt3O4_IW5gnA0c58eXshg" youtube) ; 8-Bit Keys
  ("craig1black" youtube)              ; Adrian's Digital Basement
  ("UCcAlTqd9zID6aNX3TzwxJXg" youtube) ; The Art of Code
  ("UCk_9hHaw0n1Vbbld6cXyh6A" youtube) ; Brooklyn Duo
  ("damo2986" youtube)
  ("destinws2" youtube)
  ("EEVblog" youtube)
  ("eevblog2" youtube)
  ("UCkGvUEt8iQLmq3aJIMjT2qQ" youtube) ; EEVdiscover
  ("UCWXCrItCF6ZgXrdozUS-Idw" youtube) ; ExplosmEntertainment
  ("FilmTheorists" youtube)
  ("UCHYoe8kQ-7Gn9ASOlmI0k6Q" youtube) ; Food Theory
  ("foodwishes" youtube)
  ("UCfVFSjHQ57zyxajhhRc7i0g" youtube) ; GameHut
  ("UCtWCNdtCS-SG2gKYaYhE7BA" youtube) ; Gaming Jay
  ("GetDaved" youtube)
  ("UCuCkxoKLYO_EQ2GeFtbM_bw" youtube) ; Half as Interesting
  ("henders007" youtube) ; Grand Illusions
  ("gurneyjourney" youtube)
  ("UCErSSa3CaP_GJxmFpdjG9Jw" youtube) ; Lessons from the Screenplay
  ("UCm9K6rby98W8JigLoZOh6FQ" youtube) ; LockPickingLawyer
  ("jastownsendandson" youtube)
  ("MatthewPatrick13" youtube)
  ("MatthiasWandel" youtube)
  ("UC3_AWXcf2K3l9ILVuQe-XwQ" youtube) ; Matthias random stuff
  ("NCommander" youtube)
  ("Nerdwriter1" youtube)
  ("numberphile" youtube)
  ("patrickhwillems" youtube)
  ("Pixelmusement" youtube)
  ("PlumpHelmetPunk" youtube)
  ("UCs8DNFOxYen3kuj87aWKG9g" youtube) ; Primitive Skills
  ("ProZD" youtube)
  ("XboxAhoy" youtube)
  ("Cercopithecan" youtube) ; Sebastian Lague
  ("ShamusYoung" youtube)
  ("standupmaths" youtube)
  ("UCO8DQrSp5yEP937qNqTooOw" youtube) ; Strange Parts
  ("UCy0tKL1T7wFoYcxCe0xjN6Q" youtube) ; Technology Connections
  ("UClRwC5Vc8HrB6vGx6Ti-lhA" youtube) ; Technology Connextras
  ("Thunderf00t" youtube)
  ("UCqrrxZeeFSNCjGmD-33SKMw" youtube) ; u m a m i
  ("handmadeheroarchive" youtube dev)
  ("UCwRqWnW5ZkVaP_lZF7caZ-g" youtube) ; Retro Game Mechanics Explained
  ("phreakindee" youtube)
  ("UCCj_mkYyeGIb9MPSdb74ykA" youtube) ; GET OFF MY LAWN
  ("szyzyg" youtube)
  ("UCsXVk37bltHxD1rDPwtNM8Q" youtube) ; Kurzgesagt – In a Nutshell
  ("WelkerFarmsInc" youtube)
  ("Wendoverproductions" youtube))

(provide 'feed-setup)

;;; feed-setup.el ends here

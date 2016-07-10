;;; feed-setup.el --- customize my web feeds

(require 'cl-lib)
(require 'elfeed)
(require 'youtube-dl-mode)

(setq-default elfeed-search-filter "-junk @1-week-ago +unread")

;; More keybindings

(define-key elfeed-search-mode-map "h"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter (default-value 'elfeed-search-filter))))

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

;; youtube-dl config

(setq youtube-dl-directory "~/netshare")

(defface elfeed-youtube
  '((t :foreground "#f9f"))
  "Marks YouTube videos in Elfeed."
  :group 'elfeed)

(push '(youtube elfeed-youtube)
      elfeed-search-face-alist)

(defun elfeed-show-youtube-dl ()
  "Download the current entry with youtube-dl."
  (interactive)
  (pop-to-buffer (youtube-dl-download (elfeed-entry-link elfeed-show-entry))))

(defun elfeed-search-youtube-dl ()
  "Download the current entry with youtube-dl."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (dolist (entry entries)
      (if (null (youtube-dl-download (elfeed-entry-link entry)))
          (message "Entry is not a YouTube link!")
        (message "Downloading %s" (elfeed-entry-title entry)))
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless (use-region-p) (forward-line)))))

(defun elfeed-search-youtube-comments ()
  (interactive)
  (dolist (entry (elfeed-search-selected))
    (let ((url (elfeed-entry-link entry)))
      (browse-url (replace-regexp-in-string "/watch" "/all_comments" url)))))

(define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
(define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
(define-key elfeed-search-mode-map "c" 'elfeed-search-youtube-comments)

;; Special filters

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "3 days ago"
                              :remove 'unread))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "JimKB"
                              :entry-link '(not "\/r\/comics\/")
                              :add 'junk
                              :remove 'unread))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :entry-title "^upandoutcomic:"
                              :add 'junk))

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
  ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
  ("http://accidental-art.tumblr.com/rss" image math)
  ("http://www.askamanager.org/feed" blog)
  ("http://english.bouletcorp.com/feed/" comic)
  ("http://feeds.feedburner.com/amazingsuperpowers" comic)
  ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
  ("http://bit-player.org/feed" blog math)
  ("http://blakeembrey.com/feed.xml" blog dev)
  ("http://simblob.blogspot.com/feeds/posts/default" blog dev)
  ("http://blog.carlosgaldino.com/atom.xml" blog dev)
  ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
  ("https://blog.coinbase.com/rss/" product bitcoin)
  ("http://www.commitstrip.com/en/feed/" comic dev)
  ("http://feeds.feedburner.com/Buttersafe" comic)
  ("http://feeds.feedburner.com/CatVersusHuman" comic)
  ("http://chainsawsuit.com/feed/" comic)
  ("http://feeds.feedburner.com/channelATE" comic)
  ("http://deep-dark-fears.tumblr.com/rss" comic)
  ("http://crawl.develz.org/wordpress/feed" blog gaming product)
  ("https://www.blogger.com/feeds/19727420/posts/default" blog)
  ("https://www.debian.org/security/dsa" debian list security important)
  ("http://dvdp.tumblr.com/rss" image)
  ("https://www.digitalocean.com/blog/feed" blog product)
  ("http://bay12games.com/dwarves/dev_now.rss" blog gaming product)
  ("http://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
  ("http://emacshorrors.com/feed.atom" blog emacs)
  ("http://emacsredux.com/atom.xml" blog dev emacs)
  ("http://embedsysweekly.com/feed/" dev newsletter)
  ("http://feeds.exploringbinary.com/ExploringBinary" blog dev)
  ("http://feeds.feedburner.com/Explosm" comic)
  ("http://www.extrafabulouscomics.com/1/feed" comic)
  ("http://www.exocomics.com/feed" comic)
  ("http://www.tedunangst.com/flak/rss" dev blog)
  ("http://feeds.feedburner.com/Pidjin" comic)
  ("http://thegentlemansarmchair.com/feed/" comic)
  ("http://www.goneintorapture.com/rss" comic)
  ("http://www.businesscat.happyjar.com/feed/" comic)
  ("http://feeds.feedburner.com/InvisibleBread" comic)
  ("http://blog.ioactive.com/feeds/posts/default" blog security)
  ("http://irreal.org/blog/?feed=rss2" blog emacs)
  ("http://blog.reverberate.org/feeds/posts/default" dev blog)
  ("http://feeds.feedburner.com/lefthandedtoons/awesome" comic)
  ("http://gottwurfelt.wordpress.com/feed/" blog math)
  ("http://feeds.feedburner.com/LoadingArtist" comic)
  ("http://www.lunarbaboon.com/comics/rss.xml" comic)
  ("https://www.masteringemacs.org/feed" blog emacs)
  ("http://www.ma3comic.com/comic.rss" comic)
  ("http://www.mazelog.com/rss" math puzzle)
  ("http://mercworks.net/feed/" comic)
  ("http://www.mrlovenstein.com/rss.xml" comic)
  ("http://mortoray.com/feed/" blog dev)
  ("http://mycardboardlife.com/feed" comic)
  ("http://nedroid.com/feed/" comic)
  ("http://nklein.com/feed/" blog dev)
  ("http://www.npccomic.com/feed/" comic)
  ("http://nullprogram.com/feed/" blog dev myself)
  ("https://blogs.msdn.microsoft.com/oldnewthing/feed" blog dev)
  ("http://www.optipess.com/feed/" comic)
  ("http://owlturd.com/rss" comic)
  ("http://piecomic.tumblr.com/rss" comic)
  ("http://planet.emacsen.org/atom.xml" emacs planet)
  ("http://possiblywrong.wordpress.com/feed/" blog math puzzle)
  ("http://preshing.com/feed" dev blog)
  ("http://feeds.wnyc.org/radiolab" audio)
  ("http://redpanels.com/rss/redpanelsRSS.xml" comic)
  ("http://www.safelyendangered.com/feed/" comic)
  ("https://www.schneier.com/blog/atom.xml" blog security)
  ("http://www.smbc-comics.com/rss.php" comic)
  ;;("https://haneefmubarak.com/rss/" dev blog) ; Emacs bug #20465
  ("http://feeds.feedburner.com/spaceavalanche1" comic)
  ("http://www.howstuffworks.com/podcasts/stuff-you-should-know.rss" audio)
  ("http://thecodelesscode.com/rss" dev story)
  ("https://github.com/blog/all.atom" blog dev product)
  ("http://blog.plover.com/index.atom" blog dev)
  ("http://use-the-index-luke.com/blog/feed" blog dev databases)
  ("http://modern-sql.com/feed" blog dev databases)
  ("http://www.thingsinsquares.com/feed/" comic)
  ("http://towerdive.com/feed/" blog)
  ("http://www.shamusyoung.com/twentysidedtale/?feed=rss2" blog gaming)
  ("http://upandoutcomic.tumblr.com/rss" comic)
  ("http://blog.vivekhaldar.com/rss" blog)
  ("http://what-if.xkcd.com/feed.atom" blog)
  ("http://www.whompcomic.com/rss.php" comic)
  ("http://wordsmith.org/awad/rss1.xml" word)
  ("http://blag.xkcd.com/feed/" blog)
  ("http://xkcd.com/atom.xml" comic)
  ("http://www.reddit.com/domain/nullprogram.com.rss" reddit myself)
  ("http://www.reddit.com/r/dailyprogrammer/.rss" subreddit)
  ("http://www.reddit.com/user/JimKB/submitted.rss" comic)
  ("UCLfDq-ReBz9heEGqyT3EVnQ" youtube)
  ("1veritasium" youtube)
  ("Campster" youtube)
  ("Computerphile" youtube)
  ("DemolitionRanch" youtube)
  ("destinws2" youtube)
  ("engineerguyvideo" youtube)
  ("FilmTheorists" youtube)
  ("foodwishes" youtube)
  ("GetDaved" youtube)
  ("GhazPlays" youtube)
  ("KiteTales" youtube)
  ("lseths" youtube)
  ("jastownsendandson" youtube)
  ("JonTronShow" youtube)
  ("MatthewPatrick13" youtube)
  ("MatthiasWandel" youtube)
  ("24hrMrLocksmith" youtube)
  ("PlumpHelmetPunk" youtube)
  ("Thunderf00t" youtube)
  ("ZombieOrpheusEnt" youtube)
  ("ZoochosisCom" youtube)
  ("friendznet" youtube)
  ("handmadeheroarchive" youtube dev)
  ("jefmajor" youtube)
  ("phreakindee" youtube)
  ("quill18" youtube)
  ("szyzyg" youtube)
  ("UCsXVk37bltHxD1rDPwtNM8Q" youtube)) ; Kurzgesagt – In a Nutshell


(provide 'feed-setup)

;;; feed-setup.el ends here

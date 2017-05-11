;;; feed-setup.el --- customize my web feeds

(require 'cl-lib)
(require 'elfeed)
(require 'youtube-dl)

(setq-default elfeed-search-filter "-junk @1-week-ago +unread")

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

(defun elfeed-search-youtube-comments ()
  (interactive)
  (dolist (entry (elfeed-search-selected))
    (let ((url (elfeed-entry-link entry)))
      (browse-url (replace-regexp-in-string "/watch" "/all_comments" url)))))

(define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
(define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
(define-key elfeed-search-mode-map "D" 'elfeed-search-youtube-dl-slow)
(define-key elfeed-search-mode-map "L" 'youtube-dl-list)
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
  ("https://sanctum.geek.nz/arabesque/feed/" blog dev)
  ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
  ("http://accidental-art.tumblr.com/rss" image math)
  ("https://www.npr.org/rss/podcast.php?id=510299" audio)
  ("http://english.bouletcorp.com/feed/" comic)
  ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
  ("http://bit-player.org/feed" blog math)
  ("http://simblob.blogspot.com/feeds/posts/default" blog dev)
  ("http://blog.carlosgaldino.com/atom.xml" blog dev)
  ("https://utcc.utoronto.ca/~cks/space/blog/?atom" blog dev)
  ("https://blog.coinbase.com/rss/" product bitcoin)
  ("http://www.commitstrip.com/en/feed/" comic dev)
  ("http://www.bitercomics.com/feed/" comic)
  ("http://feeds.feedburner.com/Buttersafe" comic)
  ("http://feeds.feedburner.com/CatVersusHuman" comic)
  ("http://chainsawsuit.com/feed/" comic)
  ("http://feeds.feedburner.com/channelATE" comic)
  ("http://deep-dark-fears.tumblr.com/rss" comic)
  ("https://www.blogger.com/feeds/19727420/posts/default" blog)
  ("https://www.debian.org/security/dsa" debian list security important)
  ("https://www.debian.org/News/news" debian list)
  ("http://dvdp.tumblr.com/rss" image)
  ("https://www.digitalocean.com/blog/feed" blog product)
  ("http://bay12games.com/dwarves/dev_now.rss" blog gaming product)
  ("http://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
  ("http://emacshorrors.com/feed.atom" blog emacs)
  ("http://feeds.exploringbinary.com/ExploringBinary" blog dev)
  ("http://feeds.feedburner.com/Explosm" comic)
  ("http://www.extrafabulouscomics.com/1/feed" comic)
  ("http://www.exocomics.com/feed" comic)
  ("https://www.factorio.com/blog/rss" blog dev gaming product)
  ("http://www.tedunangst.com/flak/rss" dev blog)
  ("https://flapenguin.me/atom.xml" dev blog)
  ("http://firefly.nu/feeds/all.atom.xml" blog dev)
  ("http://feeds.feedburner.com/Pidjin" comic)
  ("http://www.goneintorapture.com/rss" comic)
  ("http://www.businesscat.happyjar.com/feed/" comic)
  ("http://feeds.feedburner.com/InvisibleBread" comic)
  ("http://blog.ioactive.com/feeds/posts/default" blog security)
  ("http://irreal.org/blog/?feed=rss2" blog)
  ("http://blog.reverberate.org/feeds/posts/default" dev blog)
  ("http://feeds.feedburner.com/lefthandedtoons/awesome" comic)
  ("http://gottwurfelt.wordpress.com/feed/" blog math)
  ("http://feeds.feedburner.com/LoadingArtist" comic)
  ("https://www.masteringemacs.org/feed" blog emacs)
  ("http://www.mazelog.com/rss" math puzzle)
  ("http://www.mrlovenstein.com/rss.xml" comic)
  ("http://mortoray.com/feed/" blog dev)
  ("http://nedroid.com/feed/" comic)
  ("https://nickdesaulniers.github.io/atom.xml" blog dev)
  ("http://nullprogram.com/feed/" blog dev myself)
  ("https://blogs.msdn.microsoft.com/oldnewthing/feed" blog dev)
  ("http://www.optipess.com/feed/" comic)
  ("http://piecomic.tumblr.com/rss" comic)
  ("http://planet.emacsen.org/atom.xml" emacs planet)
  ("http://possiblywrong.wordpress.com/feed/" blog math puzzle)
  ("http://feeds.wnyc.org/radiolab" audio)
  ("http://www.safelyendangered.com/feed/" comic)
  ("https://www.schneier.com/blog/atom.xml" blog security)
  ("http://www.smbc-comics.com/rss.php" comic)
  ("http://www.howstuffworks.com/podcasts/stuff-you-should-know.rss" audio)
  ("https://github.com/blog/all.atom" blog dev product)
  ("http://blog.plover.com/index.atom" blog dev)
  ("http://use-the-index-luke.com/blog/feed" blog dev databases)
  ("http://slatestarcodex.com/feed/" blog philosophy)
  ("http://www.thingsinsquares.com/feed/" comic)
  ("http://www.shamusyoung.com/twentysidedtale/?feed=rss2" blog gaming)
  ("http://what-if.xkcd.com/feed.atom" blog)
  ("http://www.whompcomic.com/rss.php" comic)
  ("http://xkcd.com/atom.xml" comic)
  ("http://hnapp.com/rss?q=host:nullprogram.com" hackernews myself)
  ("http://www.reddit.com/domain/nullprogram.com.rss" reddit myself)
  ("http://www.reddit.com/r/dailyprogrammer/.rss" subreddit)
  ("http://www.reddit.com/user/JimKB/submitted.rss" comic)
  ("1veritasium" youtube)
  ("UCYO_jab_esuFRV4b17AJtAw" youtube) ; 3Blue1Brown
  ("adric22" youtube) ; The 8-bit Guy
  ("damo2986" youtube)
  ("DemolitionRanch" youtube)
  ("destinws2" youtube)
  ("everyframeapainting" youtube)
  ("FilmTheorists" youtube)
  ("foodwishes" youtube)
  ("GetDaved" youtube)
  ("GhazPlays" youtube)
  ("whoisjimmy" youtube) ; How Ridiculous
  ("KiteTales" youtube)
  ("UCXNxwOuuR7LT-SkEfOJiwgA" youtube) ; Long Plays
  ("jastownsendandson" youtube)
  ("JonTronShow" youtube)
  ("MatthewPatrick13" youtube)
  ("MatthiasWandel" youtube)
  ("PlumpHelmetPunk" youtube)
  ("ProZD" youtube)
  ("UCO8DQrSp5yEP937qNqTooOw" youtube) ; Strange Parts
  ("Thunderf00t" youtube)
  ("handmadeheroarchive" youtube dev)
  ("UCwRqWnW5ZkVaP_lZF7caZ-g" youtube) ; Retro Game Mechanics Explained
  ("phreakindee" youtube)
  ("quill18" youtube)
  ("sciencium" youtube)
  ("szyzyg" youtube)
  ("UCsXVk37bltHxD1rDPwtNM8Q" youtube) ; Kurzgesagt – In a Nutshell
  ("Wendoverproductions" youtube))

(provide 'feed-setup)

;;; feed-setup.el ends here

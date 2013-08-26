;;; feed-setup.el --- customize my web feeds

(require 'elfeed)
(require 'youtube-dl-mode)

(add-hook 'elfeed-new-entry-hook
          (elfeed-regexp-tagger "youtube\\.com" 'youtube))

(add-hook 'elfeed-new-entry-hook
          (elfeed-time-untagger "1 week ago" 'unread))

;; youtube-dl config

(setq youtube-dl-directory "/media/wellons")

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

(define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
(define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)

;; The actual feeds listing

(setq elfeed-feeds
      '("http://threesixty360.wordpress.com/feed/"
        "http://www.50ply.com/atom.xml"
        "http://abstrusegoose.com/feed.xml"
        "http://curiousprogrammer.wordpress.com/feed/"
        "http://feeds.feedburner.com/amazingsuperpowers"
        "http://amitp.blogspot.com/feeds/posts/default"
        "http://theatticlight.net/atom.xml"
        "http://pages.cs.wisc.edu/~psilord/blog/rssfeed.rss"
        "http://www.anticscomic.com/?feed=rss2"
        "http://feeds.feedburner.com/blogspot/TPQSS"
        "http://www.rsspect.com/rss/asw.xml"
        "http://beardfluff.com/feed/"
        "http://bit-player.org/feed"
        "http://feeds.feedburner.com/bitquabit"
        "http://simblob.blogspot.com/feeds/posts/default"
        "http://blogofholding.com/?feed=rss2"
        "http://demonastery.org/rss.php"
        "http://feeds.feedburner.com/Buttersafe"
        "http://feeds.feedburner.com/CatVersusHuman"
        "http://chainsawsuit.com/feed/"
        "http://feeds.feedburner.com/channelATE"
        "http://www.cheeseburgersinthesky.com/feed/"
        "http://feeds.feedburner.com/codeincomplete"
        "http://lisptips.com/rss"
        "http://completelyseriouscomics.com/?feed=rss2"
        "http://feeds.feedburner.com/cowbirdsinlove"
        "http://feeds.feedburner.com/DamnCoolPics"
        "http://echosa.github.io/atom.xml"
        "http://www.devrand.org/feeds/posts/default"
        "http://random.terminally-incoherent.com/rss"
        "http://feeds.feedburner.com/wordpress/divisbyzero"
        "http://dorophone.blogspot.com/feeds/posts/default"
        "http://feeds.feedburner.com/DUIF"
        "http://crawl.develz.org/wordpress/feed"
        "http://dvdp.tumblr.com/rss"
        "http://bay12games.com/dwarves/dev_now.rss"
        "http://www.soa-world.de/echelon/feed"
        "http://emacs-fu.blogspot.com/feeds/posts/default"
        "http://www.ericsink.com/rss.xml"
        "http://feeds.feedburner.com/Explosm"
        "http://www.extrafabulouscomics.com/1/feed"
        "http://exocomics.com/feed"
        "http://feeds.feedburner.com/Pidjin"
        "http://inkwellideas.com/feed/"
        "http://feeds.feedburner.com/InvisibleBread"
        "http://blog.ioactive.com/feeds/posts/default"
        "http://irreal.org/blog/?feed=rss2"
        "http://feeds.feedburner.com/JoelKirchartz"
        "http://jorgetavares.com/feed/"
        "http://feeds.feedburner.com/kinokofryfeed"
        "http://www.leadpaintcomics.com/feed/"
        "http://feeds.feedburner.com/lefthandedtoons/awesome"
        "http://gottwurfelt.wordpress.com/feed/"
        "http://loldwell.com/?feed=rss2"
        "http://www.malloc47.com/rss.xml"
        "http://maneggs.com/feed/"
        "http://www.masteringemacs.org/feed/"
        "http://www.ma3comic.com/comic.rss"
        "http://www.mercworks.net/feed/"
        "http://feeds.feedburner.com/MimiAndEunice"
        "http://mrdiv.tumblr.com/rss"
        "http://www.mrlovenstein.com/rss.xml"
        "http://mycardboardlife.com/feed"
        "http://nedroid.com/feed/"
        "http://nklein.com/feed/"
        "http://feeds.feedburner.com/NpcComic"
        "http://nullprogram.com/blog/index.rss"
        "http://feeds.feedburner.com/Optipess"
        "http://pandyland.net/feed/"
        "http://www.rsspect.com/rss/pfsc.xml"
        "http://possiblywrong.wordpress.com/feed/"
        "http://problemtown.com/feed/"
        "http://feeds.wnyc.org/radiolab"
        "http://raganwald.com/atom.xml"
        "http://research.swtch.com/feed.atom"
        "http://feeds.feedburner.com/RichardWisemansBlog"
        "http://feeds.feedburner.com/ReasonableDeviations"
        "http://feeds.feedburner.com/rolang"
        "http://www.safelyendangered.com/feed/"
        "http://www.schneier.com/blog/atom.xml"
        "http://sea-of-memes.com/rss.xml"
        "http://seemikedraw.com.au/feed"
        "http://www.skullsecurity.org/blog/feed"
        "http://feeds.feedburner.com/smbc-comics/PvLb"
        "http://sorrycomics.blogspot.com/feeds/posts/default"
        "http://feeds.feedburner.com/spaceavalanche1"
        "http://stevelosh.com/feed/"
        "http://storyboardcomics.blogspot.com/feeds/posts/default"
        "http://www.terminally-incoherent.com/blog/feed/"
        "http://feeds.feedburner.com/thebrads"
        "http://thecodelesscode.com/rss"
        "https://github.com/blog.atom"
        "http://blog.theoldreader.com/rss"
        "http://feeds.feedburner.com/thetechnium"
        "http://blog.plover.com/index.atom"
        "http://notch.tumblr.com/rss"
        "http://batsov.com/atom.xml"
        "http://towerdive.com/feed/"
        "http://www.shamusyoung.com/twentysidedtale/?feed=rss2"
        "http://blog.vivekhaldar.com/rss"
        "http://what-if.xkcd.com/feed.atom"
        "http://whattheemacsd.com/atom.xml"
        "http://www.whompcomic.com/category/comics/feed/"
        "http://blag.xkcd.com/feed/"
        "http://xkcd.com/atom.xml"
        "http://gdata.youtube.com/feeds/base/users/Base14Productions/uploads"
        "http://gdata.youtube.com/feeds/base/users/BattleBunny1979/uploads"
        "http://gdata.youtube.com/feeds/base/users/BlueXephos/uploads"
        "http://gdata.youtube.com/feeds/base/users/Briarstoned/uploads"
        "http://gdata.youtube.com/feeds/base/users/Campster/uploads"
        "http://gdata.youtube.com/feeds/base/users/DonkeyPuncher1976/uploads"
        "http://gdata.youtube.com/feeds/base/users/GetDaved/uploads"
        "http://gdata.youtube.com/feeds/base/users/GhazPlays/uploads"
        "http://gdata.youtube.com/feeds/base/users/HuntrBlackLuna/uploads"
        "http://gdata.youtube.com/feeds/base/users/JonTronShow/uploads"
        "http://gdata.youtube.com/feeds/base/users/MatthiasWandel/uploads"
        "http://gdata.youtube.com/feeds/base/users/Mestherion/uploads"
        "http://gdata.youtube.com/feeds/base/users/MrUnderlay/uploads"
        "http://gdata.youtube.com/feeds/base/users/PlumpHelmetPunk/uploads"
        "http://gdata.youtube.com/feeds/base/users/Scruit/uploads"
        "http://gdata.youtube.com/feeds/base/users/Vihart/uploads"
        "http://gdata.youtube.com/feeds/base/users/ZombieOrpheusEnt/uploads"
        "http://gdata.youtube.com/feeds/base/users/ZoochosisCom/uploads"
        "http://gdata.youtube.com/feeds/base/users/davidr64yt/uploads"
        "http://gdata.youtube.com/feeds/base/users/eEconomics/uploads"
        "http://gdata.youtube.com/feeds/base/users/emacsrocks/uploads"
        "http://gdata.youtube.com/feeds/base/users/engineerguyvideo/uploads"
        "http://gdata.youtube.com/feeds/base/users/friendznet/uploads"
        "http://gdata.youtube.com/feeds/base/users/jefmajor/uploads"
        "http://gdata.youtube.com/feeds/base/users/kmgpsu/uploads"
        "http://gdata.youtube.com/feeds/base/users/phreakindee/uploads"
        "http://gdata.youtube.com/feeds/base/users/praxgirl/uploads"
        "http://gdata.youtube.com/feeds/base/users/quill18/uploads"
        "http://gdata.youtube.com/feeds/base/users/skiptherules/uploads"
        "http://gdata.youtube.com/feeds/base/users/zzandr1o/uploads"))

(provide 'feed-setup)

;;; feed-setup.el ends here

;;; zenburn-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 cew.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme zenburn
  "Copied over from Daniel Brockman's https://github.com/dbrock/zenburn-el; needs further customization.")

(custom-theme-set-faces
 'zenburn
 '(default ((t (:background "#3f3f3f" :foreground "#dcdccc"))))
 '(cursor ((t (:background "#dcdccc" :foreground "#3f3f3f"))))
 '(region ((t (:background "#233323" :foreground "#71d3b4"))))
 '(mode-line ((t (:background "#1e2320" :foreground "#acbc90"))))
 '(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090"))))
 '(fringe ((t (:background "#464646"))))
 '(minibuffer-prompt ((t (:foreground "#f0dfaf"))))
 '(font-lock-builtin-face ((t (:foreground "#8cd0d3"))))
 '(font-lock-comment-face ((t (:foreground "#7f9f7f"))))
 '(font-lock-constant-face ((t (:foreground "#dca3a3" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#f0dfaf"))))
 '(font-lock-keyword-face ((t (:foreground "#f0dfaf" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#cc9393"))))
 '(font-lock-type-face ((t (:foreground "#dfdfbf" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#f0dfaf"))))
 '(font-lock-warning-face ((t (:background "#332323" :foreground "#e37170"))))
 '(isearch ((t (:background "#506070" :foreground "#dcdccc"))))
 '(lazy-highlight ((t (:background "#1e2320" :foreground "#dcdccc"))))
 '(link ((t (:foreground "#f0dfaf" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#2e3330" :foreground "#88b090")))))

(provide-theme 'zenburn)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zenburn-theme.el  ends here

;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Emacs package archives
;; (setq personal-package-archives 'tencent)

;; Github token string for eaf-markdown-previewer
(setq personal-eaf-grip-token nil)

;; Flagship themes
;;   `doom-one'
;;   `doom-one-light'
;;   `doom-vibrant'
;;
;; Additional themes
;;   [X] `doom-acario-dark' (added by gagbo)
;;   [X] `doom-acario-light' (added by gagbo)
;;   [X] `doom-city-lights' (added by fuxialexnder)
;;   [X] `doom-challenger-deep' (added by fuxialexnder)
;;   [X] `doom-dracula' (added by fuxialexnder)
;;   [X] `doom-fairy-floss' (added by ema2159)
;;   [X] `doom-gruvbox' (added by JongW)
;;   [X] `doom-Iosvkem' (added by neutaaaaan)
;;   [X] `doom-laserwave' (added by hyakt)
;;   [X] `doom-molokai'
;;   [X] `doom-moonlight' (added by Brettm12345)
;;   [X] `doom-nord' (added by fuxialexnder)
;;   [X] `doom-nord-light' (added by fuxialexnder)
;;   [X] `doom-nova' (added by bigardone)
;;   [X] `doom-oceanic-next' (added by juanwolf)
;;   [X] `doom-opera' (added by jwintz)
;;   [X] `doom-opera-light' (added by jwintz)
;;   [X] `doom-outrun' (added by ema2159)
;;   [X] `doom-palenight' (added by Brettm12345)
;;   [X] `doom-peacock' (added by teesloane)
;;   [X] `doom-solarized-dark' (added by ema2159)
;;   [X] `doom-solarized-light' (added by fuxialexnder)
;;   [X] `doom-sourcerer' (added by defphil)
;;   [X] `doom-spacegrey' (added by teesloane)
;;   [X] `doom-tomorrow-night'
;;   [X] `doom-tomorrow-day'
;;   [X] `doom-wilmersdorf'
;;   [ ] `doom-mono-dark' / `doom-mono-light'
;;   [ ] `doom-tron'
(setq personal-gui-theme 'doom-solarized-light)
(setq personal-tui-theme 'doom-solarized-light)

;; Rss feeds
(setq personal-elfeed-feeds
      '(("https://emacs-china.org/latest.rss" emacs china)
        ("https://emacs-china.org/posts.rss" emacs china)))

;; frame startup size, 'max or 'fullscreen
(setq personal-frame-startup-size 'max)

;; "Shell used in `term' and `ansi-term'."
(setq personal-shell-executable "/usr/bin/zsh")

;; Org page config
(with-eval-after-load 'org-page
  ;; put your configs here
  (setq op/theme 'mdo))

;;; custom.el ends here

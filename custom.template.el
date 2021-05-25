;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Emacs package archives
;; (setq personal-package-archives 'tencent)

;; Github token string for eaf-markdown-previewer
(setq personal-eaf-grip-token nil)

;; `doom-themes'
(setq personal-gui-theme 'doom-solarized-light)
(setq personal-tui-theme 'doom-dark+)

;; Rss feeds
(setq elfeed-feeds
      '(("http://sachachua.com/blog/category/emacs/feed/" Sacha-Chua)))

;; frame startup size, 'max or 'fullscreen
(setq personal-frame-startup-size 'fullscreen)

;; "Shell used in `term' and `ansi-term'."
(setq personal-shell-executable "/usr/bin/zsh")


;;; custom.el ends here

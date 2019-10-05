;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Emacs package archives
;; (setq personal-package-archives 'tencent)

;; Github token string for eaf-markdown-previewer
(setq personal-eaf-grip-token nil)

;; Emacs theme "doom-vibrant" "doom-nord-light"
(setq personal-gui-theme 'doom-nord-light)

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

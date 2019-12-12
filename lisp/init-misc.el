;; init-misc.el --- Some other things	 -*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Some other things
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Rss reader
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (when personal-elfeed-feeds
    (setq elfeed-feeds personal-elfeed-feeds))
  (when (featurep 'evil-collection)
    (evil-collection-init 'elfeed)))

;; Youdao
(use-package youdao-dictionary
  :ensure t
  :commands (youdao-dictionary-search-at-point+
             youdao-dictionary-search-at-point-tooltip
             youdao-dictionary-play-voice-at-point)
  :config
  (setq url-automatic-caching t)
  (with-eval-after-load 'evil
    (evil-define-key 'normal youdao-dictionary-mode-map "q" 'quit-window)))

;; Markdowm
(with-eval-after-load 'markdown-mode
  (defun eaf-markdown-previewer ()
    "Markdown Previewer."
    (interactive)
    (eaf-open buffer-file-name))

  (+funcs/major-mode-leader-keys
   markdown-mode-map
   "b" '(nil :which-key "block")
   "bb" '(play-code-block :which-key "play-code-with-online-playground")
   "y" '(youdao-dictionary-search-at-point-tooltip :which-key "translate-at-point")
   "v" '(youdao-dictionary-play-voice-at-point :which-key "voice-at-point")
   "p" '(eaf-markdown-previewer :which-key "previewer")
   "t" '(nil :which-key "toggle")
   "ti" '(markdown-toggle-inline-images :which-key "inline-images")))

;; This extension will ask me Chinese words and then insert translation as variable or function name.
;; https://github.com/manateelazycat/insert-translated-name
(use-package insert-translated-name
  :commands insert-translated-name-insert
  :quelpa ((insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name")))

;; Leetcode
;; https://github.com/kaiwk/leetcode.el
;; Remember to set your account and password
(use-package leetcode
  :ensure t
  :commands leetcode)

;; Http(s) Proxy
(defun proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" personal-http-proxy)
    (message "No proxy")))

(defun proxy-http-enable ()
  "Enable http/https proxy."
  (setq url-proxy-services `(("http" . ,personal-http-proxy)
                             ("https" . ,personal-http-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable http/https proxy."
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

;; Socks Proxy
(use-package socks
  :ensure nil
  :defer t
  :config
  (defun proxy-mode-socks-enable ()
    "Enable Socks proxy."
    (setq url-gateway-method 'socks)
    (setq socks-noproxy '("localhost"))
    (setq socks-server '("Default server" "localhost" 1080 5))
    (message "socks proxy %s enabled" socks-server))

  (defun proxy-mode-socks-disable ()
    "Disable Socks proxy."
    (setq url-gateway-method 'native)
    (message "socks proxy diabled")))

(defun proxy-socks-toggle ()
  "Toggle socks proxy."
  (interactive)
  (unless (featurep 'socks)
    (require 'socks))
  (if (equal url-gateway-method 'native)
      (proxy-mode-socks-enable)
    (proxy-mode-socks-disable)))

;; https://github.com/twlz0ne/with-proxy.el/tree/master
(use-package with-proxy
  :ensure t
  :commands with-proxy)

;; Music Player
(use-package bongo
  :ensure t
  :commands bongo)

(use-package powerthesaurus
  :ensure t
  :commands (powerthesaurus-lookup-word
             powerthesaurus-lookup-word-at-point
             powerthesaurus-lookup-word-dwim))

;; Use nmcli to manage network
(defvar counsel-network-manager-history nil
  "Network manager history.")

(defun counsel-network-manager (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "nmcli device wifi rescan")
  (let ((networks-list (s-split "\n" (shell-command-to-string "nmcli device wifi"))))
    (ivy-read "Select network" networks-list
              :initial-input initial-input
              :require-match t
              :history counsel-network-manager-history
              :sort nil
              :caller 'counsel-network-manager
              :action (lambda (line)
                        (let ((network (car (s-split " " (s-trim (s-chop-prefix "*" line)) t))))
                          (message "Connecting to \"%s\".." network)
                          (async-shell-command
                           (format "nmcli device wifi connect %s"
                                   (shell-quote-argument network))))))))

(use-package webkit-color-picker
  :ensure t
  :commands (webkit-color-picker-show))

;; Major mode for crontab(5) files
(use-package crontab-mode
  :ensure t
  :mode ("\\.?cron\\(tab\\)?\\'" . crontab-mode))

;; User can use curl when s/he has it, as curl is more reliable
;; than url.el.
(use-package request
  :ensure t
  :defer t)


(provide 'init-misc)

;;; init-misc.el ends here

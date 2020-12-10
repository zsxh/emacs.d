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
  :commands elfeed
  :config
  (when personal-elfeed-feeds
    (setq elfeed-feeds personal-elfeed-feeds))
  (when (featurep 'evil-collection)
    (evil-collection-init 'elfeed)))

;; Youdao
(use-package youdao-dictionary
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
   "bb" '(playonline :which-key "play-code-with-online-playground")
   "y" '(youdao-dictionary-search-at-point-tooltip :which-key "translate-at-point")
   "v" '(youdao-dictionary-play-voice-at-point :which-key "voice-at-point")
   "p" '(eaf-markdown-previewer :which-key "previewer")
   "t" '(nil :which-key "toggle")
   "ti" '(markdown-toggle-inline-images :which-key "inline-images")))

;; https://github.com/lorniu/go-translate
;; TODO: google translate token issue
(use-package go-translate
  :commands (go-translate go-translate-popup)
  :config
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-base-url "https://translate.google.cn"))

;; This extension will ask me Chinese words and then insert translation as variable or function name.
;; https://github.com/manateelazycat/insert-translated-name
(use-package insert-translated-name
  :commands insert-translated-name-insert
  :quelpa (insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name"))

;; Leetcode
;; https://github.com/kaiwk/leetcode.el
;; Remember to set your account and password
(use-package leetcode
  :commands leetcode)

;; Http(s) Proxy
(defun proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is '%s:%s'" personal-proxy-http-host personal-proxy-http-port)
    (message "No proxy")))

(defun proxy-http-enable ()
  "Enable http/https proxy."
  (setq url-proxy-services `(("http" . ,(format "%s:%s" personal-proxy-http-host personal-proxy-http-port))
                             ("https" . ,(format "%s:%s" personal-proxy-http-host personal-proxy-http-port))
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
    (setq socks-server '("Default server" "localhost" ,personal-proxy-socks5-port 5))
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
  :commands with-proxy)

;; Music Player
(use-package bongo
  :commands bongo)

(use-package powerthesaurus
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

;; Major mode for crontab(5) files
(use-package crontab-mode
  :mode ("\\.?cron\\(tab\\)?\\'" . crontab-mode))

;; User can use curl when s/he has it, as curl is more reliable
;; than url.el.
(use-package request
  :defer t)

;; TODO: deft-mode, notdeft, take notes
;; https://emacs-china.org/t/v1/8218/53

;; Install lilypond and add it to load-path
;; $pacman -S lilypond
(use-package lilypond-mode
  :if (and (executable-find "lilypond")
           (file-exists-p "/usr/share/emacs/site-lisp/lilypond-mode.el"))
  :load-path "/usr/share/emacs/site-lisp"
  :commands LilyPond-mode
  :mode (("\\.ly$" . LilyPond-mode)
         ("\\.ily$" . LilyPond-mode)))

;; https://github.com/benma/visual-regexp.el
(use-package visual-regexp
  :defer t)

;; https://github.com/twlz0ne/playonline.el
;; `playonline-block' (require org-mode / markdown)
(use-package playonline
  :commands (playonline))

;; Usage:
;; emacs -batch -l ${package-elpa-dir}/elisp-benchmarks.el -f elisp-benchmarks-run
(use-package elisp-benchmarks
  :defer t)

(use-package memory-usage
  :commands memory-usage)

;; TODO: View Large Files
;; https://github.com/m00natic/vlfi
(use-package vlf
  :defer t)

(use-package netease-cloud-music
  :if (executable-find "mplayer")
  :load-path (lambda () (expand-file-name "submodules/netease-cloud-music" user-emacs-directory))
  :commands (netease-cloud-music))

;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
(use-package keycast
  :defer t
  :config
  (with-eval-after-load 'keycast
    (define-minor-mode keycast-mode
      "Show current command and its key binding in the mode line."
      :global t
      (if keycast-mode
          (progn
            (add-hook 'pre-command-hook 'keycast-mode-line-update t)
            (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
        (remove-hook 'pre-command-hook 'keycast-mode-line-update)
        (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string))))))

;; https://github.com/joostkremers/writeroom-mode
;; `writeroom-mode'
(use-package writeroom-mode
  :defer t
  :custom
  (writeroom-width 106))

;; archlinuxcn repo
;; > pacman -S telegram-tdlib
(use-package telega
  :defer t
  :config
  (setq telega-proxies (list '(:server "127.0.0.1" :port 1081 :enable t :type (:@type "proxyTypeHttp"))
                             '(:server "127.0.0.1" :port 1080 :enable nil :type (:@type "proxyTypeSocks5"))))

  (with-eval-after-load 'telega-msg
    (define-key telega-msg-button-map (kbd "k") nil)
    (define-key telega-msg-button-map (kbd "l") nil))

  (with-eval-after-load 'telega-chat
    (when evil-mode
      (evil-define-key 'normal telega-chat-mode-map "q" #'kill-current-buffer)))

  (with-eval-after-load 'telega-ins
    ;; customize date format
    (defun telega-ins--date-a (timestamp)
      "Insert TIMESTAMP.
Format is:
- HH:MM      if today
- Mon/Tue/.. if on this week
- YYYY/MM/DD otherwise"
      (let* ((dtime (decode-time timestamp))
             (current-ts (time-to-seconds (current-time)))
             (ctime (decode-time current-ts))
             (today00 (telega--time-at00 current-ts ctime)))
        (if (and (> timestamp today00)
                 (< timestamp (+ today00 (* 24 60 60))))
            (telega-ins-fmt "%02d:%02d" (nth 2 dtime) (nth 1 dtime))

          (let* ((week-day (nth 6 ctime))
                 (mdays (+ week-day
                           (- (if (< week-day telega-week-start-day) 7 0)
                              telega-week-start-day)))
                 (week-start00 (telega--time-at00
                                (- current-ts (* mdays 24 3600)))))
            (if (and (> timestamp week-start00)
                     (< timestamp (+ week-start00 (* 7 24 60 60))))
                (telega-ins (nth (nth 6 dtime) telega-week-day-names))

              (telega-ins-fmt "%02d/%02d/%02d"
                (nth 5 dtime) (nth 4 dtime) (nth 3 dtime)))))))
    (advice-add #'telega-ins--date :override #'telega-ins--date-a)))


(provide 'init-misc)

;;; init-misc.el ends here

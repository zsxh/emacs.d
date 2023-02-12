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
  (setq elfeed-curl-extra-arguments `(,(format "-xhttp://%s:%s" personal-proxy-http-host personal-proxy-http-port))
        elfeed-log-level 'debug)

  (when (featurep 'evil-collection)
    (evil-collection-init 'elfeed))

  (require 'eaf)
  (require 'elfeed-show)

  (defun +eaf/elfeed-current-window ()
    (interactive)
    (let ((entry (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        ;; Move to next feed item.
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (unless elfeed-search-remain-on-entry (forward-line))
        ;; Open elfeed item in current window
        (eaf-open-browser (elfeed-entry-link entry)))))

  (evil-define-key 'normal elfeed-search-mode-map (kbd "<return>") '+eaf/elfeed-current-window))

;; Youdao
(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point+
             youdao-dictionary-search-at-point-tooltip
             youdao-dictionary-play-voice-at-point)
  :custom
  (youdao-dictionary-search-history-file (locate-user-emacs-file "cache/youdao-history"))
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
(use-package go-translate
  :commands (go-translate go-translate-popup)
  :config
  (setq gts-translate-list '(("en" "zh"))))

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
    (setq socks-server `("Default server" "localhost" ,personal-proxy-socks5-port 5))
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

;; Use nmcli to manage network
(defvar nmcli-network-manager-history nil
  "Network manager history.")

;; $ nmcli con delete <SSID> # if "Secrets were required, but not provided", and you already offer a password
;; $ nmcli device wifi list
;; $ nmcli device wifi connect <SSID> --ask
(defun nmcli-network-manager (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "nmcli device wifi rescan")
  (let ((networks-list (s-split "\n" (shell-command-to-string "nmcli device wifi")))
        (line (consult--read networks-list
                             :prompt "Select network"
                             :initial initial-input
                             :require-match t
                             :sort nil
                             :history nmcli-network-manager-history))
        (f (lambda (line)
             (let ((network (car (s-split " " (s-trim (s-chop-prefix "*" line)) t))))
               (message "Connecting to \"%s\".." network)
               ;; NOTE: remember to insert your password in async shell buffer
               (async-shell-command
                (format "nmcli device wifi connect %s --ask"
                        (shell-quote-argument network)))))))
    (funcall 'f line)))

;; User can use curl when s/he has it, as curl is more reliable
;; than url.el.
(use-package request
  :defer t
  :custom
  (request-storage-directory (locate-user-emacs-file "cache/request")))

;; TODO: try another http client using curl as a backend, https://github.com/alphapapa/plz.el

;; Usage:
;; emacs -batch -l ${package-elpa-dir}/elisp-benchmarks.el -f elisp-benchmarks-run
(use-package elisp-benchmarks
  :defer t)

(use-package memory-usage
  :commands memory-usage)

;; https://zevlg.github.io/telega.el/#building-tdlib
;; $ git clone --depth 1 https://github.com/tdlib/td
;; $ cd td
;; $ mkdir -p build && cd build && cmake ../
;; $ make -j8
;; $ sudo make install
;; `telega-server-libs-prefix'
;; M-x `telega-server-build'
(use-package telega
  :defer t
  :config
  (setq telega-proxies (list '(:server "127.0.0.1" :port 1081 :enable t :type (:@type "proxyTypeHttp"))
                             '(:server "127.0.0.1" :port 1080 :enable nil :type (:@type "proxyTypeSocks5")))
        telega-old-date-format "%Y/%M/%D"
        telega-translate-to-language-by-default "zh")

  ;; avatar size
  (setf (alist-get 2 telega-avatar-factors-alist ) '(0.4 . 0.1))

  (with-eval-after-load 'telega-root
    (with-eval-after-load 'evil
      (evil-define-key 'normal telega-root-mode-map "Q" #'telega-kill)))

  (with-eval-after-load 'telega-msg
    (define-key telega-msg-button-map (kbd "k") nil)
    (define-key telega-msg-button-map (kbd "l") nil))

  (with-eval-after-load 'telega-chat

    (define-key telega-chat-button-map (kbd "h") nil)
    (with-eval-after-load 'evil
      (evil-define-key 'normal telega-chat-mode-map "q" #'kill-current-buffer)
      (define-key telega-msg-button-map (kbd "SPC") nil))

    (defun my-telega-chat-mode ()
      (set (make-local-variable 'company-backends)
           (append (list telega-emoji-company-backend
                         'telega-company-username
                         'telega-company-hashtag)
                   (when (telega-chat-bot-p telega-chatbuf--chat)
                     '(telega-company-botcmd))))
      (company-mode 1))
    (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode))

  (with-eval-after-load 'all-the-icons
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-root-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-chat-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-blue))))

;; https://github.com/tecosaur/screenshot.git
;; convert a selected region of code to a screenshot
(use-package screenshot
  :ensure nil
  :init (slot/vc-install :fetcher "github" :repo "tecosaur/screenshot")
  :commands screenshot)

;; NOTE: timers
;; (defmacro nasy/timer (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))

;; (nasy/timer (format-mode-line mode-line-format))

(use-package protobuf-mode
  :defer t)

(use-package esup
  :defer t)

;; TODO: https://github.com/ginqi7/plantuml-emacs


(provide 'init-misc)

;;; init-misc.el ends here

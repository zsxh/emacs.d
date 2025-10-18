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
  (require 'elfeed-show)
  (setq elfeed-curl-extra-arguments `(,(format "-xhttp://%s:%s" personal-proxy-http-host personal-proxy-http-port))
        elfeed-log-level 'debug)
  ;; https://github.com/skeeto/elfeed?tab=readme-ov-file#filter-syntax
  (setq-default elfeed-search-filter "@6-months-ago +unread")

  ;; Entries older than 6 months are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "6 months ago"
                                :remove 'unread))

  (with-eval-after-load 'evil-collection
    (evil-collection-init 'elfeed)))

(use-package elfeed-webkit
  :if (featurep 'xwidget-internal)
  :after elfeed
  :bind (:map elfeed-show-mode-map
         ("%" . elfeed-webkit-toggle)))

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

;; https://github.com/lorniu/go-translate
(use-package go-translate
  :commands (gt-do-translate gt-do-speak)
  :config
  (setq gt-langs '("en" "zh")
        gt-default-translator (gt-translator
                               :engines (gt-google-engine)
                               :render (gt-buffer-render)))
  (with-eval-after-load 'evil
    (add-hook 'gt-buffer-render-init-hook
              (lambda ()
                (evil-define-key '(normal visual insert emacs) gt-buffer-render-local-map
                  "q" 'kill-buffer-and-window)))))

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

;; Usage:
;; emacs -batch -l ${package-elpa-dir}/elisp-benchmarks.el -f elisp-benchmarks-run
(use-package elisp-benchmarks
  :defer t)

(use-package memory-usage
  :commands memory-usage)

;; https://github.com/tecosaur/screenshot.git
;; convert a selected region of code to a screenshot
(use-package screenshot
  :vc (:url "https://github.com/tecosaur/screenshot")
  :commands screenshot)

;; NOTE: timers
;; (defmacro nasy/timer (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))

;; (nasy/timer (format-mode-line mode-line-format))

;; use "C-\" to `toggle-input-method'
(use-package rime
  :if IS-LINUX
  :defer t
  :init
  (advice-run-once 'toggle-input-method :before (lambda (&rest _) (require 'rime)))
  :custom
  (default-input-method "rime")
  :config
  (when (package-installed-p 'posframe))
  (setq rime-show-candidate 'posframe))

;; Monitoring Linux Journald logs
;; https://github.com/WJCFerguson/journalctl
(use-package journalctl-mode
  :defer t
  :commands (journalctl))

;; TODO: password manager
(use-package pass
  :defer t
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-pass-setup)))

(use-package keycast
  :defer t)

;; Emacs editing PostgreSQL databases
(use-package pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs")
  :defer t)

;; `show-font-list', `show-font-select-preview',`show-font-tabulated'
(use-package show-font
  :defer t)


(provide 'init-misc)

;;; init-misc.el ends here

;; init-telega.el --- Telega Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Telega Configurations
;;

;;; Code:

;; NOTE: How to send code block
;; https://zevlg.github.io/telega.el/#sending-ordinary-messages

;; Install `emacsPackages.telega' via nix to obtain `telega-server'
(use-package telega
  :defer t
  :init
  (setq telega-directory (expand-file-name (locate-user-emacs-file "cache/telega")))
  :config
  ;; (require 'telega-bridge-bot)
  (require 'telega-transient nil t)

  (setq telega-proxies (list `(:server ,personal-proxy-http-host :port ,personal-proxy-http-port :enable nil :type (:@type "proxyTypeHttp"))
                             `(:server ,personal-proxy-http-host :port ,personal-proxy-socks5-port :enable t :type (:@type "proxyTypeSocks5")))
        telega-translate-to-language-by-default "zh"
        telega-date-format-alist '((today . "%H:%M")
                                   (this-week . "%a")
                                   (old . "%Y.%m.%d")
                                   (date . "%Y.%m.%d")
                                   (time . "%H:%M")
                                   (date-time . "%Y.%m.%d %a %H:%M")
                                   (date-long . "%Y %B %d")
                                   (date-break-bar . "%Y %B %d %a"))
        ;; avatar
        telega-avatar-workaround-gaps-for '(return t))

  (with-eval-after-load 'evil
    (evil-define-key* 'normal telega-root-mode-map
      "j" #'telega-button-forward
      "k" #'telega-button-backward
      "v" #'telega-transient-root-view
      "F" #'telega-transient-chat-folder
      "Q" #'telega-kill
      "?" telega-root-mode-map))

  (with-eval-after-load 'telega-msg
    (define-key telega-msg-button-map (kbd "k") nil)
    (define-key telega-msg-button-map (kbd "l") nil))

  (with-eval-after-load 'telega-chat

    (define-key telega-chat-button-map (kbd "h") nil)
    (with-eval-after-load 'evil
      (evil-define-key 'normal telega-chat-mode-map "q" #'kill-current-buffer)
      (define-key telega-msg-button-map (kbd "SPC") nil))))


(provide 'init-telega)

;;; init-telega.el ends here

;; init-telega.el --- Telega Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Telega Configurations
;;

;;; Code:

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
  ;; (require 'telega-bridge-bot)
  ;; (require 'telega-transient)

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
                                   (date-break-bar . "%Y %B %d %a")))

  ;; avatar
  (setq telega-avatar-workaround-gaps-for '(return t))

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
      (define-key telega-msg-button-map (kbd "SPC") nil)))

  (with-eval-after-load 'nerd-icons
    (push '(telega-root-mode nerd-icons-faicon "nf-fae-telegram" :face nerd-icons-blue) nerd-icons-mode-icon-alist)
    (push '(telega-chat-mode nerd-icons-faicon "nf-fae-telegram" :face nerd-icons-blue) nerd-icons-mode-icon-alist)))


(provide 'init-telega)

;;; init-telega.el ends here

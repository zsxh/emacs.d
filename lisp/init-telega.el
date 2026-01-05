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

  ;; Highlight code blocks inside messages
  (require 'telega-mnz)
  (add-hook 'telega-load-hook 'global-telega-mnz-mode)

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
      "v" telega-root-view-map
      "F" telega-folder-map
      "Q" #'telega-kill
      "?" telega-root-mode-map))

  (with-eval-after-load 'telega-msg
    (define-key telega-msg-button-map (kbd "k") nil)
    (define-key telega-msg-button-map (kbd "l") nil))

  (with-eval-after-load 'telega-chat

    (define-key telega-chat-button-map (kbd "h") nil)
    (with-eval-after-load 'evil
      (evil-define-key 'normal telega-chat-mode-map "q" #'kill-current-buffer)
      (define-key telega-msg-button-map (kbd "SPC") nil)))

  (with-eval-after-load 'telega-company
    (advice-add #'telega-company-botcmd :override
                (lambda (command &optional arg &rest _ignored)
                  (interactive (list 'interactive))
                  (cl-case command
                    (interactive (company-begin-backend 'telega-company-botcmd))
                    (require-match 'never)
                    (sorted t)
                    (prefix
                     (telega-company-grab-botcmd))
                    (candidates
                     (all-completions arg (telega-company--bot-commands)))
                    (annotation
                     (get-text-property 0 'telega-annotation arg))))))

  ;; Completing input in chatbuf
  ;; https://zevlg.github.io/telega.el/#completing-input-in-chatbuf
  (when (require 'company nil t)
    (defun +telega/completion-setup ()
      (setq-local completion-at-point-functions (mapcar #'cape-company-to-capf telega-company-backends))
      (corfu-mode 1))
    (add-hook 'telega-chat-mode-hook #'+telega/completion-setup))

  ;; `telega-mnz-attach-region-as-code'
  (+funcs/major-mode-leader-keys
   telega-chat-mode-map
   "c" '(telega-mnz-chatbuf-attach-code :which-key "telega-mnz-chatbuf-attach-code")))


(provide 'init-telega)

;;; init-telega.el ends here

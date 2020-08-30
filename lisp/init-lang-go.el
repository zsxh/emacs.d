;; init-lang-go.el --- Go Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Go Lang Config
;;

;;; Code:

;; Install gopls
;; https://github.com/golang/tools/blob/master/gopls/doc/user.md#installation
(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . lsp-deferred)
  :config
  (require 'lsp-go)
  (+language-server/set-common-leader-keys go-mode-map))

(use-package lsp-go
  :defer t
  :ensure lsp-mode
  :config
  (dolist (go-env-name '("GO111MODULE" "GOPROXY" "GOPATH"))
    (if-let ((value (getenv go-env-name)))
        (puthash go-env-name value lsp-go-env))))


(provide 'init-lang-go)

;;; init-lang-go.el ends here

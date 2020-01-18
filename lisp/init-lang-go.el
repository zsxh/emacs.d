;; init-lang-go.el --- Go Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Go Lang Config
;;

;;; Code:

;; require go language server `bingo'
;; https://github.com/saibing/bingo/wiki/Install
(use-package go-mode
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :hook (go-mode . lsp)
  :config
  (+language-server/set-common-leader-keys go-mode-map))


(provide 'init-lang-go)

;;; init-lang-go.el ends here

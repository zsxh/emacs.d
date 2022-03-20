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
  :hook (go-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys go-mode-map)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))


(provide 'init-lang-go)

;;; init-lang-go.el ends here

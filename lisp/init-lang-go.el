;; init-lang-go.el --- Go Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Go Lang Config
;;

;;; Code:

;; NOTE: Install gopls

(use-package go-mode
  :if (not (treesit-ready-p 'go))
  :hook (go-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys go-mode-map)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(use-package go-ts-mode
  :ensure nil
  :if (treesit-ready-p 'go)
  :hook (go-ts-mode . eglot-ensure)
  :config
  (setq go-ts-mode-indent-offset 4)
  (+eglot/set-leader-keys go-ts-mode-map)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(with-eval-after-load 'eglot
  ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (defun +go/workspace-configuration (&optional server)
    '(:gopls
      (:usePlaceholders t
       :hints (:assignVariableTypes t
               :compositeLiteralFields t
               :compositeLiteralTypes t
               :constantValues t
               :functionTypeParameters t
               :parameterNames t
               :rangeVariableTypes t)
       :staticcheck t
       :gofumpt t)))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode go-mode))
    (+go/workspace-configuration))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode go-ts-mode))
    (+go/workspace-configuration)))


(provide 'init-lang-go)

;;; init-lang-go.el ends here

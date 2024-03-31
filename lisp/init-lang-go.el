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
  :if (not (treesit-ready-p 'go))
  ;; :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys go-mode-map)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(use-package go-ts-mode
  :ensure nil
  :if (treesit-ready-p 'go)
  ;; :mode ("\\.go\\'" . go-ts-mode)
  :hook (go-ts-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys go-ts-mode-map)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(with-eval-after-load 'eglot
  ;; https://github.com/joaotavora/eglot/discussions/1369#discussioncomment-8779782
  (defun +go/workspace-configuration (&optional server)
    '(:gopls
      (:hints
       (:parameterNames t))))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode go-mode))
    (+go/workspace-configuration))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode go-ts-mode))
    (+go/workspace-configuration)))


(provide 'init-lang-go)

;;; init-lang-go.el ends here

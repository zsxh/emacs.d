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
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY" "GOROOT"))))

(use-package go-ts-mode
  :ensure nil
  :if (treesit-ready-p 'go)
  :hook (go-ts-mode . eglot-ensure)
  :bind (:map go-ts-mode-map
         ("<f1>" . go-tools-menu))
  :config
  (setq go-ts-mode-indent-offset 4)
  (+eglot/set-leader-keys go-ts-mode-map)
  (modify-syntax-entry ?\" "\"" go-ts-mode--syntax-table)
  (modify-syntax-entry ?` "\"" go-ts-mode--syntax-table)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY" "GOROOT")))

  (transient-define-prefix go-tools-menu ()
    ""
    [["misc"
      ("d" "godoc" godoc)
      ("i" "go-impl" go-impl)
      ("s" "fillstruct" go-fill-struct)
      ("t" "add-tag" go-tag-add)
      ("T" "remove-tag" go-tag-remove)]
     ["test"
      ("g" "gen-test" go-gen-test-dwim)
      ("f" "test-current-file" go-test-current-file)
      ("x" "test-current-test" go-test-current-test)
      ("p" "test-current-project" go-test-current-project)
      ("b" "test-current-benchmark" go-test-current-benchmark)
      ("c" "test-current-coverage" go-test-current-coverage)
      ("r" "go-run" go-run)]])

  (+funcs/major-mode-leader-keys
   go-ts-mode-map
   "m" '(go-tools-menu :which-key "go-tools-menu")))

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

(use-package go-impl :defer t)          ;; NOTE: `completion-styles' should be `basic'
(use-package go-fill-struct :defer t)   ;; TODO: fillstruct already provided by gopls
(use-package go-tag
  :defer t
  :init (setq go-tag-args (list "-transform" "camelcase")))
(use-package go-gen-test :defer t)
(use-package gotest :defer t)


(provide 'init-lang-go)

;;; init-lang-go.el ends here

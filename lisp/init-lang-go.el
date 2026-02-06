;; init-lang-go.el --- Go Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Go Lang Config
;;

;;; Code:

;; NOTE: Install gopls

;; TODO: gopls codelens
;; https://deepwiki.com/search/-gopls-codelensvscodego-codele_bd0ae6ba-3738-4b40-bd4d-b7848d4f0edb?mode=fast
;; https://deepwiki.com/search/-gopls-codelens-codelens-comma_e9f152d7-583a-41a2-a8fa-09b8d95aee0c?mode=fast
;; https://github.com/golang/vscode-go/blob/master/docs/settings.md#uicodelenses

(when (and (version< emacs-version "31")
           (treesit-ready-p 'go))
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-dot-mod-mode . go-mod-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-dot-work-mode . go-work-ts-mode)))

(use-package go-mode
  :hook (go-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys go-mode-map)
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY" "GOROOT"))))

(use-package go-ts-mode
  :ensure nil
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

(use-package eglot-gopls
  :vc (:url "https://github.com/zsxh/eglot-gopls")
  :after eglot
  :config
  (push '((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode go-work-ts-mode)
          . (eglot-gopls-server . ("gopls")))
        eglot-server-programs)

  ;; NOTE: https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (defvar eglot-go-workspace-configuration
    '(:gopls (;; --- Build ---
              ;; --- Formatting ---
              :gofumpt t
              ;; --- UI ---
              :codelenses (:generate t
                           :regenerate_cgo t
                           :tidy t
                           :upgrade_dependency t
                           :vendor t
                           :test t
                           ;; :vulncheck t
                           :run_govulncheck t)
              :semanticTokens t
              ;; --- Completion ---
              :usePlaceholders t
              ;; --- Diagnostic ---
              ;; :staticcheck t
              ;; --- Documentation ---
              ;; --- Inlayhint ---
              :hints (:assignVariableTypes t
                      :compositeLiteralFields t
                      :compositeLiteralTypes t
                      :constantValues t
                      :functionTypeParameters t
                      :parameterNames t
                      :rangeVariableTypes t)
              ;; --- Navigation ---
              )))
  (setq-default eglot-workspace-configuration
                (plist-put eglot-workspace-configuration :gopls
                           (plist-get eglot-go-workspace-configuration :gopls))))

(use-package go-impl :defer t)          ;; NOTE: `completion-styles' should be `basic'
(use-package go-fill-struct :defer t)   ;; TODO: fillstruct already provided by gopls
(use-package go-tag
  :defer t
  :init (setq go-tag-args (list "-transform" "camelcase")))
(use-package go-gen-test :defer t)
(use-package gotest :defer t)


(provide 'init-lang-go)

;;; init-lang-go.el ends here

;; init-lang-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Julia Lang Configurations
;;

;;; Code:

(require 'init-language-server)

;; Install Julia LanguageServer
;; $ julia
;; julia> ]
;; pkg> add LanguageServer
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :hook ((julia-mode . (lambda ()
                         (setq-local lsp-enable-folding t
                                     lsp-folding-range-limit 100)
                         (lsp-deferred)))
         (julia-mode . julia-repl-mode))
  :custom (julia-indent-offset 2)
  :config
  (require 'lsp-julia)
  (+language-server/set-common-leader-keys julia-mode-map))

;; FIXME: SymbolServer.jl takes a very long time to process project dependencies
;; https://github.com/julia-vscode/SymbolServer.jl/issues/56
;; This is a one-time process that shouldn’t cause issues once the dependencies are cached,
;; however it can take over a minute to process each dependency.
(use-package lsp-julia
  :defer t
  :custom
  (lsp-julia-package-dir nil)     ; use the globally installed version
  (lsp-julia-default-environment "~/.julia/environments/v1.4")
  :config
  (defun +julia/lsp-get-root (dir)
    "Get the (Julia) project root directory of the current file."
    (expand-file-name (if dir (or (locate-dominating-file dir "JuliaProject.toml")
                                  (locate-dominating-file dir "Project.toml")
                                  lsp-julia-default-environment)
                        lsp-julia-default-environment)))

  (advice-add 'lsp-julia--get-root :override
              (lambda ()
                (+julia/lsp-get-root (buffer-file-name)))))

(use-package julia-repl
  :commands julia-repl-mode)


(provide 'init-lang-julia)

;;; init-lang-julia.el ends here

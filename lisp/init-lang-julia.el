;; init-lang-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Julia Lang Configurations
;;

;;; Code:

(require 'init-language-server)

;; FIXME: LanguageServer V2.0 broken now
;; https://github.com/JuliaEditorSupport/LanguageServer.jl/issues/300
;; Install Julia LanguageServer
;; $ julia
;; julia> ]
;; pkg> add CSTParser StaticLint DocumentFormat SymbolServer LanguageServer
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :hook ((julia-mode . lsp-deferred)
         (julia-mode . julia-repl-mode))
  :custom (julia-indent-offset 2)
  :config (+language-server/set-common-leader-keys julia-mode-map))

;; FIXME: SymbolServer.jl takes a very long time to process project dependencies
;; https://github.com/julia-vscode/SymbolServer.jl/issues/56
;; This is a one-time process that shouldn’t cause issues once the dependencies are cached,
;; however it can take over a minute to process each dependency.
(use-package lsp-julia
  :after julia-mode
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

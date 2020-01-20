;; init-lang-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Julia Lang Configurations
;;

;;; Code:

(require 'init-language-server)

;; https://github.com/JuliaEditorSupport/LanguageServer.jl/issues/300
;; Install Julia LanguageServer
;; $ julia
;; julia> ]
;; (v1.3) pkg> add CSTParser#master StaticLint#master DocumentFormat#master SymbolServer#master LanguageServer#master
(use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode)
  :hook ((julia-mode . lsp)
         (julia-mode . julia-repl-mode))
  :config
  (setq julia-indent-offset 2))

;; for lsp
(use-package lsp-julia
  :after julia-mode
  :config
  (setq lsp-julia-package-dir nil) ; use the globally installed version
  (setq lsp-julia-default-environment "~/.julia/environments/v1.3")
  (defun lsp-julia--get-root (dir)
    "Get the (Julia) project root directory of the current file."
    (expand-file-name (if dir (or (locate-dominating-file dir "JuliaProject.toml")
                                  (locate-dominating-file dir "Project.toml")
                                  lsp-julia-default-environment)
                        lsp-julia-default-environment)))
  (defun lsp-julia--rls-command ()
    "The command to lauch the Julia Language Server."
    `(,lsp-julia-command
      ,@lsp-julia-flags
      ,(concat "-e using LanguageServer, Sockets, SymbolServer;"
               " server = LanguageServerInstance("
               " stdin, stdout, false,"
               " \"" (lsp-julia--get-root (buffer-file-name)) "\","
               " \"" (lsp-julia--get-depot-path) "\","
               "Dict());"
               " run(server);"))))

(use-package julia-repl
  :commands julia-repl-mode)

(with-eval-after-load 'julia-mode
  (+language-server/set-common-leader-keys julia-mode-map))


(provide 'init-lang-julia)

;;; init-lang-julia.el ends here

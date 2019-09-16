;; init-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

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
;; $> julia
;; julia> ]
;; (v1.0) pkg> add LanguageServer#master StaticLint#master DocumentFormat#master SymbolServer#master
(use-package julia-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
  :config
  (setq julia-indent-offset 2))

;; for lsp
(use-package lsp-julia
  :after julia-mode
  :quelpa ((lsp-julia :fetcher github :repo "non-Jedi/lsp-julia"))
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.2"))

(add-hook 'julia-mode-hook 'lsp)

(use-package julia-repl
  :ensure t
  :commands julia-repl-mode
  :hook (julia-mode . julia-repl-mode))

;; for eglot

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(julia-mode . ("julia"
;;                                "--startup-file=no"
;;                                "--history-file=no"
;;                                "-e"
;;                                "using LanguageServer, Sockets, SymbolServer; server = LanguageServer.LanguageServerInstance(stdin, stdout, false, \"~./julia/enviroments/v1.0\", \"\", Dict()); run(server);"))))
;; (add-hook 'julia-mode-hook 'eglot-ensure)


(provide 'init-julia)

;;; init-julia.el ends here

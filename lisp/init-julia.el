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
;; (v1.0) pkg> add CSTParser#master StaticLint#master DocumentFormat#master SymbolServer#master LanguageServer#master
(use-package julia-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
  :config
  (setq julia-indent-offset 2))

;; for lsp
(use-package lsp-julia
  :ensure t
  :after julia-mode
  :config
  (setq lsp-julia-package-dir nil)      ; use the globally installed version
  (setq lsp-julia-default-environment "~/.julia/environments/v1.3"))

(add-hook 'julia-mode-hook 'lsp)

(use-package julia-repl
  :ensure t
  :commands julia-repl-mode
  :hook (julia-mode . julia-repl-mode))

(with-eval-after-load 'lsp-julia
  (+language-server/set-common-leader-keys julia-mode-map))


(provide 'init-julia)

;;; init-julia.el ends here

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
  :defer t)

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

;; for lsp

(defcustom lsp-julia-default-environment "~/.julia/environments/v1.0"
  "The path to the default environment."
  :type 'string
  :group 'lsp-julia)

(defcustom lsp-julia-command "julia"
  "Command to invoke julia with."
  :type 'string
  :group 'lsp-julia)

(defcustom lsp-julia-flags '("--startup-file=no" "--history-file=no")
  "List of additional flags to call julia with."
  :type '(repeat (string :tag "argument"))
  :group 'lsp-julia)

(defcustom lsp-julia-timeout 30
  "Time before lsp-mode should assume julia just ain't gonna start."
  :type 'number
  :group 'lsp-julia)

(defun lsp-julia--get-root ()
  (let ((dir (locate-dominating-file default-directory "Project.toml")))
    (if dir (expand-file-name dir)
      (expand-file-name lsp-julia-default-environment))))

(defun lsp-julia--rls-command ()
  `(,lsp-julia-command
    ,@lsp-julia-flags
    ,(concat "-e using LanguageServer, Sockets, SymbolServer;"
             " server = LanguageServer.LanguageServerInstance("
             " stdin, stdout, false,"
             " \"" (lsp-julia--get-root) "\","
             " \"\", Dict());"
             " server.runlinter = true;"
             " run(server);")))

(defconst lsp-julia--handlers
  '(("window/setStatusBusy" .
     (lambda (w _p)))
    ("window/setStatusReady" .
     (lambda(w _p)))))

(with-eval-after-load 'lsp-clients
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-julia--rls-command)
                    :major-modes '(julia-mode)
                    :server-id 'julia-ls)))


(provide 'init-julia)

;;; init-julia.el ends here

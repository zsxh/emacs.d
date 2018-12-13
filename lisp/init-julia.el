;; init-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Julia Lang Configurations
;;

;;; Code:

(use-package julia-mode
  :ensure t
  :defer t)

(use-package julia-repl
  :ensure t
  :commands julia-repl-mode
  :hook (julia-mode . julia-repl-mode))

;; for eglot

;; (require 'init-eglot)

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(julia-mode . ("julia"
;;                                "--startup-file=no"
;;                                "--history-file=no"
;;                                "-e"
;;                                "using LanguageServer, Sockets, SymbolServer; server = LanguageServer.LanguageServerInstance(stdin, stdout, false, \"~./julia/enviroments/v1.0\", \"\", Dict()); run(server);"))))

;; (add-hook 'julia-mode-hook 'eglot-ensure)


;; for lsp

;; (require 'lsp)
;; (require 'lsp-clients)
;; ;; (defcustom lsp-clients-julia-server-command
;; ;;   "julia --startup-file=no --history-file=no -e \"using LanguageServer, Sockets, SymbolServer; server = LanguageServer.LanguageServerInstance(stdin, stdout, false, \\\"~/.julia/enviroments/v1.0\\\", \\\"\\\", Dict()); run(server);\""
;; ;;   "The julia language server executable to use."
;; ;;   :group 'lsp-julia
;; ;;   :type 'string)

;; (defcustom lsp-clients-julia-executable "julia"
;;   "The julia executable to use.
;; Leave as just the executable name to use the default behavior of
;; finding the executable with `exec-path'."
;;   :group 'lsp-julia
;;   :risky t
;;   :type 'file)

;; (defcustom lsp-clients-julia-args '()
;;   "Extra arguments for the julia executable."
;;   :group 'lsp-julia
;;   :risky t
;;   :type '(repeat string))

;; (setq lsp-clients-julia-args '("--startup-file=no"
;;                                "--history-file=no"
;;                                "-e"
;;                                "using LanguageServer, Sockets, SymbolServer; server = LanguageServer.LanguageServerInstance(stdin, stdout, false, \"~./julia/enviroments/v1.0\", \"\", Dict()); run(server);"))

;; (defun lsp-clients--julia-command ()
;;   "Generate the language server startup command."
;;   `(,lsp-clients-julia-executable ,@lsp-clients-julia-args))

;; (lsp-register-client
;;  (make-lsp-client
;;   :new-connection (lsp-stdio-connection 'lsp-clients--julia-command)
;;   :major-modes '(julia-mode)
;;   :server-id 'julia))

;; (add-hook 'julia-mode-hook (lambda ()
;;                              (lsp)
;;                              (setq-local company-backends '(company-capf))))


(provide 'init-julia)

;;; init-julia.el ends here

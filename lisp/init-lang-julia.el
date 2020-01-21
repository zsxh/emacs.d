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
  (setq julia-indent-offset 2)
  (+language-server/set-common-leader-keys julia-mode-map))

;; for lsp
(progn
  (use-package-ensure-elpa 'lsp-julia
                           '(t)
                           'nil)
  (defvar use-package--warning240
    (function
     (lambda
       (keyword err)
       (let
           ((msg
             (format "%s/%s: %s" 'lsp-julia keyword
                     (error-message-string err))))
         (display-warning 'use-package msg :error)))))
  (condition-case-unless-debug err
      (eval-after-load 'julia-mode
        '(let
             ((now
               (current-time)))
           (message "%s..." "Loading package lsp-julia")
           (prog1
               (if
                   (not
                    (require 'lsp-julia nil t))
                   (display-warning 'use-package
                                    (format "Cannot load %s" 'lsp-julia)
                                    :error)
                 (let
                     ((now
                       (current-time)))
                   (message "%s..." "Configuring package lsp-julia")
                   (prog1
                       (condition-case-unless-debug err
                           (progn
                             (setq lsp-julia-package-dir nil)
                             (setq lsp-julia-default-environment "~/.julia/environments/v1.3")
                             (defun lsp-julia--get-root
                                 (dir)
                               "Get the (Julia) project root directory of the current file."
                               (expand-file-name
                                (if dir
                                    (or
                                     (locate-dominating-file dir "JuliaProject.toml")
                                     (locate-dominating-file dir "Project.toml")
                                     lsp-julia-default-environment)
                                  lsp-julia-default-environment)))
                             (defun lsp-julia--rls-command nil "The command to lauch the Julia Language Server."
                                    `(,lsp-julia-command ,@lsp-julia-flags ,(concat "-e using LanguageServer, Sockets, SymbolServer;" " server = LanguageServerInstance(" " stdin, stdout, false," " \""
                                                                                    (lsp-julia--get-root
                                                                                     (buffer-file-name))
                                                                                    "\"," " \""
                                                                                    (lsp-julia--get-depot-path)
                                                                                    "\"," "Dict());" " run(server);")))
                             t)
                         (error
                          (funcall use-package--warning240 :config err)))
                     (let
                         ((elapsed
                           (float-time
                            (time-subtract
                             (current-time)
                             now))))
                       (if
                           (> elapsed 0.1)
                           (message "%s...done (%.3fs)" "Configuring package lsp-julia" elapsed)
                         (message "%s...done" "Configuring package lsp-julia"))))))
             (let
                 ((elapsed
                   (float-time
                    (time-subtract
                     (current-time)
                     now))))
               (if
                   (> elapsed 0.1)
                   (message "%s...done (%.3fs)" "Loading package lsp-julia" elapsed)
                 (message "%s...done" "Loading package lsp-julia"))))))
    (error
     (funcall use-package--warning240 :catch err))))

(use-package julia-repl
  :commands julia-repl-mode)


(provide 'init-lang-julia)

;;; init-lang-julia.el ends here

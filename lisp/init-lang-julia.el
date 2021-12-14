;; init-lang-julia.el --- Julia Lang Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Julia Lang Configurations
;;

;;; Code:

(require 'init-lsp)

;; Julia PkgServer/Mirrors https://discourse.juliacn.com/t/topic/2969
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
  :config
  (setq julia-indent-offset 2)

  (require 'lsp-julia)
  (+language-server/set-common-leader-keys julia-mode-map)
  (+funcs/major-mode-leader-keys julia-mode-map
                                 "'" '(+julia/repl-vterm :which-key "repl"))

  ;; Code from https://github.com/ronisbr/doom-emacs/blob/develop/modules/lang/julia/config.el
  ;; Borrow matlab.el's fontification of math operators. From
  ;; <https://ogbe.net/emacsconfig.html>
  (dolist (mode '(julia-mode ess-julia-mode))
    (font-lock-add-keywords
     mode
     `((,(let ((OR "\\|"))
           (concat "\\("          ; stolen `matlab.el' operators first
                   ;; `:` defines a symbol in Julia and must not be highlighted
                   ;; as an operator. The only operators that start with `:` are
                   ;; `:<` and `::`. This must be defined before `<`.
                   "[:<]:" OR
                   "[<>]=?" OR
                   "\\.[/*^']" OR
                   "===" OR
                   "==" OR
                   "=>" OR
                   "\\<xor\\>" OR
                   "[-+*\\/^&|$]=?" OR ; this has to come before next (updating operators)
                   "[-^&|*+\\/~]" OR
                   ;; Julia variables and names can have `!`. Thus, `!` must be
                   ;; highlighted as a single operator only in some
                   ;; circumstances. However, full support can only be
                   ;; implemented by a full parser. Thus, here, we will handle
                   ;; only the simple cases.
                   "[[:space:]]!=?=?" OR "^!=?=?" OR
                   ;; The other math operators that starts with `!`.
                   ;; more extra julia operators follow
                   "[%$]" OR
                   ;; bitwise operators
                   ">>>" OR ">>" OR "<<" OR
                   ">>>=" OR ">>" OR "<<" OR
                   "\\)"))
        1 font-lock-type-face)))))

(use-package lsp-julia
  :load-path (lambda () (expand-file-name "submodules/lsp-julia" user-emacs-directory))
  :defer t
  :config
  (setq lsp-julia-format-indent julia-indent-offset))

(use-package julia-repl
  :commands julia-repl-mode)

(with-eval-after-load 'julia-mode
  (defun +julia/repl-vterm ()
    (interactive)
    (let ((default-directory (+project/root t)))
      (with-current-buffer (vterm-other-window)
        (when (file-exists-p (expand-file-name "Project.toml" default-directory))
          (dolist (char (string-to-list "julia"))
            (vterm--update vterm--term (char-to-string char) nil nil nil))
          (vterm-send-return)
          (dolist (char (string-to-list "]activate ."))
            (vterm--update vterm--term (char-to-string char) nil nil nil))
          (vterm-send-return)
          (vterm-send-backspace))))))

;; TODO: julia-vterm workflow
(use-package julia-vterm
  :defer t)


(provide 'init-lang-julia)

;;; init-lang-julia.el ends here

;; init-prog.el --- Common Programing Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Common Programing Settings
;;

;;; Code:

;;;;;;;;;;;;;; DOC ;;;;;;;;;;;;;;

;; require `zeal' installation
;; zeal (dash for linux)
(use-package zeal-at-point
  :ensure t
  :commands zeal-at-point)

;; require `zeal' or `dash' docsets
;; helm-dash
(use-package helm-dash
  :ensure t
  :commands (helm-dash helm-dash-at-point)
  :preface
  (setq helm-dash-docsets-path (expand-file-name "~/.local/share/Zeal/Zeal/docsets"))
  ;; (setq helm-dash-browser-func 'eww)
  (setq helm-dash-browser-func 'eaf-open-url)
  :config
  (setq helm-dash-common-docsets (helm-dash-installed-docsets)))

;;;;;;;;;;;;;; EDIT ;;;;;;;;;;;;;;

;; Structured editing
(use-package paredit
  :ensure t
  :bind (("s-0" . paredit-wrap-round)
         ("s-[" . paredit-wrap-square)
         ("s-{" . paredit-wrap-curly)
         ("s-<" . paredit-wrap-angled)
         ("s-\"" . paredit-meta-doublequote)
         ("C-M-b" . paredit-backward)
         ("C-M-f" . paredit-forward))
  :hook (prog-mode . enable-paredit-mode)
  :config
  ;; prevent whitespace between function and paren
  (defun +paredit/space-for-delimiter-p (endp delimiter)
    (or (member 'font-lock-keyword-face (text-properties-at (1- (point))))
        (not (derived-mode-p 'basic-mode
                             'c++-mode
                             'c-mode
                             'coffee-mode
                             'csharp-mode
                             'd-mode
                             'dart-mode
                             'go-mode
                             'java-mode
                             'js-mode
                             'lua-mode
                             'objc-mode
                             'pascal-mode
                             'python-mode
                             'r-mode
                             'ruby-mode
                             'rust-mode
                             'typescript-mode
                             'julia-mode))))
  (add-to-list 'paredit-space-for-delimiter-predicates '+paredit/space-for-delimiter-p))

;; Short and sweet LISP editing
(use-package lispy
  :ensure t
  :after paredit
  :commands lispy-mode
  :hook ((emacs-lisp-mode . (lambda () (lispy-mode 1)))
         (lisp-interaction-mode . (lambda () (lispy-mode 1)))
         (lisp-mode . (lambda () (lispy-mode 1))))
  :bind (:map lispy-mode-map
              ("s-k" . paredit-splice-sexp-killing-backward))
  :config
  (require 'le-lisp)
  (setq lispy-use-sly t)

  ;; Replace lispy--eavl-lisp function
  (defun lispy--eval-lisp-advice (str)
    "Eval STR as Common Lisp code."
    (let* ((deactivate-mark nil)
           (result (with-current-buffer (process-buffer (lispy--cl-process))
                     (if lispy-use-sly
                         (sly-interactive-eval str)
                       (slime-eval `(swank:eval-and-grab-output ,str))))))
      (if (equal (car result) "")
          (cadr result)
        (concat (propertize (car result)
                            'face 'font-lock-string-face)
                "\n\n"
                (cadr result)))))
  (advice-add #'lispy--eval-lisp :override #'lispy--eval-lisp-advice))

;; Change variable name style
(use-package string-inflection
  :ensure t
  :commands string-inflection-all-cycle)

;; https://gitlab.com/jgkamat/rmsbolt
;; RMSBolt tries to make it easy to see what your compiler is doing.
;; It does this by showing you the assembly output of a given source code file.
(use-package rmsbolt
  :ensure t
  :commands rmsbolt-mode)

;; Evil shift indent
(defvar prog--indent-variable-alist
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
               idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding\
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

(defun +prog/set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the\
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test prog--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(add-hook 'after-change-major-mode-hook '+prog/set-evil-shift-width)

(setq-default evil-shift-width 2)

(provide 'init-prog)

;;; init-prog.el ends here

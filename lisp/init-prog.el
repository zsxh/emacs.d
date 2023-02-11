;; init-prog.el --- Common Programing Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Common Programing Settings
;;

;;; Code:

;;;;;;;;;;;;;; Quick Scroll line ;;;;;;;;;;;;;;
;; keymap ("C-l" 'recenter-top-bottom) cycling 25%,top,bottom line position
(add-hook 'prog-mode-hook (lambda () (setq-local recenter-positions '(0.25 top bottom))))

;;;;;;;;;;;;;; DOC ;;;;;;;;;;;;;;

;;;;;;;;;;;;;; Code Folding ;;;;;;;;;;;;;;
;; evil open/close/toggle folds rely on hideshow
;; "z a" evil-toggle-fold
;; "z m" evil-close-folds
;; "z r" evil-open-folds
(use-package hideshow
  :commands hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "zf") 'hs-hide-level)))

;; https://gitlab.com/jgkamat/rmsbolt
;; RMSBolt tries to make it easy to see what your compiler is doing.
;; It does this by showing you the assembly output of a given source code file.
(use-package rmsbolt
  :commands rmsbolt-mode)

;;;;;;;;;;;;;; Indent ;;;;;;;;;;;;;;
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

(defun +prog/indent-region (numSpaces)
  (let ((regionStart (cond ((use-region-p) (region-beginning))
                           (t (line-beginning-position))))
        (regionEnd (cond ((use-region-p) (region-end))
                           (t (line-end-position)))))
    (save-excursion                  ; restore the position afterwards
      (goto-char regionStart)        ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))    ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil)        ; restore the selected region
      )))

(defun +prog/tab-region ()
  (interactive)
  (if (use-region-p)
      (+prog/indent-region 2) ; region was selected, call indent-region
    (insert "  ")              ; else insert four spaces as expected
    ))

(defun +prog/untab-region ()
  (interactive)
  (+prog/indent-region -2))

(global-set-key (kbd "<backtab>") '+prog/untab-region)

;;;;;;;;;;;;;; Coding styles for multiple developers working on the same project across various editors and IDEs ;;;;;;;;;;;;;;

(use-package editorconfig
  :hook (emacs-startup . editorconfig-mode)
  :config
  ;; use `auto-save' package to deal with trailing whitespace
  (setq editorconfig-trim-whitespaces-mode (lambda (arg) nil)))


(provide 'init-prog)

;;; init-prog.el ends here

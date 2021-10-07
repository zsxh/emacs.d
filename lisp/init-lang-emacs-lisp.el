;; init-lang-emacs-lisp.el --- Initialize Emacs Lisp Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Lisp configurations
;;

;;; Code:

;; NOTE: Elisp Guide - https://github.com/chrisdone/elisp-guide
;; Programmers who are too busy to read through long tutorials and manuals, but who want to extend their editor.
;; You don't need to learn everything from the ground up, just enough knowledge to be self-sufficient.
;; You've been using Emacs for a while and now it's time you started making some handy extensions for yourself.

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :defer t
  :bind ((:map emacs-lisp-mode-map
               ("C-c C-z" . ielm)
               ("C-c C-c" . eval-defun)
               ("C-c C-b" . eval-buffer)
               ("C-c C-:" . pp-eval-expression)
               ("C-c C-d" . edebug-defun)))
  :config
  ;; Note: '(emacs-lisp-mode-map) or (list 'emacs-lisp-mode-map)
  (dolist (mode-map '(emacs-lisp-mode-map lisp-interaction-mode-map))
    (+funcs/major-mode-leader-keys
     mode-map
     "'" '(ielm :which-key "ielm")
     "e" '(nil :which-key "eval")
     "ed" '(eval-defun :which-key "eval-defun")
     "ep" '(lispy-eval-and-comment :which-key "lispy-eval-and-comment")
     "ee" '(pp-eval-last-sexp :which-key "eval-last-sexp")
     "ej" '(eval-print-last-sexp :which-key "eval-print-last-sexp")
     "d" '(nil :which-key "debug")
     "df" '(edebug-defun :which-key "edebug-defun")
     "D" '(lispy-describe-inline :which-key "lispy-describe-inline")
     "m" '(nil :which-key "macro")
     "mc" '(pp-macroexpand-last-sexp :which-key "macroexpand-last-sexp")
     "me" '(pp-macroexpand-expression :which-key "macroexpand-expression")
     "ms" '(macrostep-expand :which-key "macrostep-expand")
     "g" '(nil :which-key "goto")
     "gd" '(evil-goto-definition :which-key "goto-definition"))))

;; Interactive macro expander
(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c e" . macrostep-expand)))
  :config
  (with-eval-after-load 'evil
    (evil-define-minor-mode-key 'normal 'macrostep-mode
      "q" 'macrostep-collapse)))

;; Short and sweet LISP editing
(use-package lispy
  :commands lispy-mode
  :bind ((:map lispy-mode-map
               (":" . self-insert-command)
               ("C-j" . newline-and-indent)))
  :hook ((lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode
          clojure-mode
          clojurec-mode
          clojurescript-mode) . lispy-mode)
  :config
  ;; this requires CIDER or cider--display-interactive-eval-result function
  (setq lispy-eval-display-style 'overlay)
  (setq lispy-use-sly t)

  (defun lispy-hash-a (orig-fn &rest args)
    (if (and (eq major-mode 'clojurescript-mode)
             (lispy-looking-back "\\[:\\sw+"))
        (insert "#")
      (apply orig-fn args)))
  (advice-add 'lispy-hash :around #'lispy-hash-a)

  (defun lispy-eval-a (orig-fn &rest args)
    (if (eq major-mode 'clojurescript-mode)
        (cider-eval-last-sexp)
      (apply orig-fn args)))
  (advice-add 'lispy-eval :around #'lispy-eval-a))

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :commands (eros-eval-last-sexp eros-eval-defun eros--make-result-overlay)
  :init
  (global-set-key [remap eval-last-sexp] #'eros-eval-last-sexp)
  (global-set-key [remap eval-defun] #'eros-eval-defun))

;; Extra font lock for emacs lisp
(use-package elispfl
  :quelpa (elispfl :fetcher github :repo "cireu/elispfl")
  :after elisp-mode
  :config
  (elispfl-mode))


(provide 'init-lang-emacs-lisp)

;;; init-lang-emacs-lisp.el ends here

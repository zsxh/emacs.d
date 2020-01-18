;; init-lang-emacs-lisp.el --- Initialize Emacs Lisp Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Lisp configurations
;;

;;; Code:

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
     "ep" '(pp-eval-expression :which-key "eval-expression")
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

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :defer t
  :diminish eldoc-mode)

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
               (":" . self-insert-command)))
  :hook ((lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode
          clojure-mode
          clojurec-mode
          clojurescript-mode) . lispy-mode)
  :config
  ;; this requires CIDER or cider--display-interactive-eval-result function
  (setq lispy-eval-display-style 'overlay)
  (defun cider--display-interactive-eval-result (value point)
    "Make overlay for VALUE at POINT."
    (eros--make-result-overlay value
                               :where point
                               :duration eros-eval-result-duration)
    (message "%s" (propertize value 'invisible nil)))

  (with-eval-after-load 'lisp-mode
    (require 'le-lisp))
  (with-eval-after-load 'clojure-mode
    (require 'le-clojure))

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

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :commands (eros-eval-last-sexp eros-eval-defun eros--make-result-overlay)
  :init
  (global-set-key [remap eval-last-sexp] #'eros-eval-last-sexp)
  (global-set-key [remap eval-defun] #'eros-eval-defun))

;; Extra font lock for emacs lisp
(use-package elispfl
  :quelpa ((elispfl :fetcher github :repo "cireu/elispfl"))
  :after elisp-mode
  :config
  (elispfl-mode))

(use-package sly-el-indent
  :quelpa ((sly-el-indent :fetcher github :repo "cireu/sly-el-indent"))
  :after elisp-mode
  :hook (emacs-lisp . sly-el-indent-setup))


(provide 'init-lang-emacs-lisp)

;;; init-lang-emacs-lisp.el ends here

;; init-emacs-lisp.el --- Initialize Emacs Lisp Configurations	-*- lexical-binding: t -*-

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
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)
              ("C-c C-:" . pp-eval-expression)
              ("C-c C-d" . edebug-defun))
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local company-backends '(company-capf company-files))))
  :config
  ;; Note: '(emacs-lisp-mode-map) or (list 'emacs-lisp-mode-map)
  (dolist (mode-map '(emacs-lisp-mode-map lisp-interaction-mode-map))
    (+funcs/set-leader-keys-for-major-mode
     mode-map
     "'" '(ielm :which-key "ielm")
     "e" '(nil :which-key "eval")
     "ed" '(eval-defun :which-key "eval-defun")
     "ep" '(pp-eval-expression :which-key "eval-expression")
     "ee" '(pp-eval-last-sexp :which-key "eval-last-sexp")
     "ej" '(eval-print-last-sexp :which-key "eval-print-last-sexp")
     "d" '(nil :which-key "debug")
     "df" '(edebug-defun :which-key "edebug-defun")
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
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)
         (lisp-interaction-mode . turn-on-eldoc-mode)
         (lisp-mode . turn-on-eldoc-mode)))

;; Interactive macro expander
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand))
  :config
  (with-eval-after-load 'evil
    (evil-define-minor-mode-key 'normal 'macrostep-mode
      "q" 'macrostep-collapse)))

;; Set lisp-interaction-mode company-backends
(add-hook 'lisp-interaction-mode-hook
          #'(lambda () (setq-local company-backends '(company-capf company-files))))

;; Short and sweet LISP editing
(use-package lispy
  :ensure t
  :commands lispy-mode
  :hook ((emacs-lisp-mode . (lambda () (lispy-mode 1)))
         (lisp-interaction-mode . (lambda () (lispy-mode 1)))
         (lisp-mode . (lambda () (lispy-mode 1))))
  :config
  ;; this requires CIDER or cider--display-interactive-eval-result function
  (setq lispy-eval-display-style 'overlay)
  (defun cider--display-interactive-eval-result (value point)
    "Make overlay for VALUE at POINT."
    (eros--make-result-overlay (format "%S" value)
      :where point
      :duration eros-eval-result-duration)
    (message "%s" (propertize value 'invisible nil)))

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

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :ensure t
  :commands (eros-eval-last-sexp eros-eval-defun eros--make-result-overlay)
  :init
  (global-set-key [remap eval-last-sexp] #'eros-eval-last-sexp)
  (global-set-key [remap eval-defun] #'eros-eval-defun))


(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here

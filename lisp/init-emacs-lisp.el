;; init-emacs-lisp.el --- Initialize Emacs Lisp Configurations	-*- lexical-binding: t -*-

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
;;  Emacs Lisp configurations
;;

;;; Code:

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)))

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Interactive macro expander
(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

;; Semantic code search for emacs lisp
(use-package elisp-refs
  :ensure t)

;; Short and sweet LISP editing
(use-package lispy
  :ensure t
  :commands lispy-mode
  :hook ((emacs-lisp-mode . (lambda () (lispy-mode 1)))
         (lisp-interaction-mode . (lambda () (lispy-mode 1)))))

;; KeyBindings
(with-eval-after-load 'elisp-mode
  (require 'general)
  (zsxh/define-major-key 'emacs-lisp-mode-map
                         "'"  '(ielm :which-key "ielm")
                         "e"  '(nil :which-key "eval")
                         "ed" '(eval-defun :which-key "eval-defun")
                         "ep" '(pp-eval-expression :which-key "eval-expression")
                         "ee" '(pp-eval-last-sexp :which-key "eval-last-sexp")
                         "ej" '(eval-print-last-sexp :which-key "eval-print-last-sexp")
                         "m"  '(nil :which-key "macro")
                         "me" '(pp-macroexpand-expression :which-key "macroexpand-expression")
                         "mc" '(pp-macroexpand-last-sexp :which-key "macroexpand-last-sexp")
                         "g"  '(nil :which-key "goto")
                         "gd" '(evil-goto-definition :which-key "goto-definition"))
  (zsxh/define-major-key 'lisp-interaction-mode-map
                         "e"  '(nil :which-key "eval")
                         "ed" '(eval-defun :which-key "eval-defun")
                         "ep" '(eval-expression :which-key "eval-expression")
                         "ee" '(eval-last-sexp :which-key "eval-last-sexp")
                         "ej" '(eval-print-last-sexp :which-key "eval-print-last-sexp")
                         "m"  '(nil :which-key "macro")
                         "me" '(pp-macroexpand-expression :which-key "macroexpand-expression")
                         "mc" '(pp-macroexpand-last-sexp :which-key "macroexpand-last-sexp")
                         "g"  '(nil :which-key "goto")
                         "gd" '(evil-goto-definition :which-key "goto-definition")))


(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here

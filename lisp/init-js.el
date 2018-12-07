;; init-js.el --- Summary	-*- lexical-binding: t -*-

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
;;  for javascript
;;

;;; Code:

(use-package lsp-javascript-typescript
  :ensure t
  :requires init-lsp
  :commands lsp-javascript-typescript-enable
  :preface
  (defun +js/lsp-configs ()
    (setq-local company-backends '((company-lsp :seperate company-yasnippet)))
    (lsp-javascript-typescript-enable))

  ;; https://github.com/emacs-lsp/lsp-javascript
  (defun my-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))

  (defun my-js-hook nil
    (make-local-variable 'company-transformers)
    (push 'my-company-transformer company-transformers))

  (add-hook 'js-mode-hook 'my-js-hook)
  :hook ((js-mode typescript-mode js3-mode rjsx-mode) . +js/lsp-configs)
  :config
  (+funcs/set-leader-keys-for-major-mode
   js-mode-map
   "f" '(lsp-format-buffer :which-key "format")
   "g" '(nil :which-key "go")
   "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
   "gr" '(lsp-ui-peek-find-references :which-key "find-references")
   "R" '(lsp-rename :which-key "rename")))


(provide 'init-js)

;;; init-js.el ends here

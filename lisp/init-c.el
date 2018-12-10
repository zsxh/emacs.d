;; init-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

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
;;  C/C++ Configurations
;;

;;; Code:

(require 'init-lsp)

;; By default files ending in .h are treated as c files rather than c++ files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package ccls
  :defer t
  :quelpa ((ccls :fetcher github :repo "MaskRay/emacs-ccls")))

(defun +c/lsp-ccls-config ()
  (require 'ccls)
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-initialization-options
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
  (when (featurep 'evil)
    (evil-set-initial-state 'ccls-tree-mode 'emacs))
  (setq-local company-backends
              '((company-lsp :separate company-yasnippet)))
  (+c/set-leader-keys)
  (lsp))

(add-hook 'c-mode-hook '+c/lsp-ccls-config)
(add-hook 'c++-mode-hook '+c/lsp-ccls-config)

(defun +c/set-leader-keys ()
  (+funcs/set-leader-keys-for-major-mode
   '(c-mode-map c++-mode-map)
   "a" '(lsp-execute-code-action :which-key "code-action")
   "e"  '(nil :which-key "error")
   "en" '(flymake-goto-next-error :which-key "next-error")
   "eN" '(flymake-goto-prev-error :which-key "prev-error")
   "f"  '(lsp-format-buffer :which-key "format")
   "g"  '(nil :which-key "go")
   "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
   "gr" '(lsp-ui-peek-find-references :which-key "find-references")
   "gh" '(ccls-member-hierarchy :which-key "member-hierarchy")
   "R"  '(lsp-rename :which-key "rename")))

(use-package cmake-mode
  :ensure t)

;; (defun +eglot/c-c++-config ()
;;   (setq-local company-backends '(company-clang))
;;   (eglot-ensure)
;;   (+funcs/set-leader-keys-for-major-mode
;;    '(c-mode-map c++-mode-map)
;;    "e"  '(nil :which-key "error")
;;    "en" '(flymake-goto-next-error :which-key "next-error")
;;    "eN" '(flymake-goto-prev-error :which-key "prev-error")
;;    "f"  '(eglot-format :which-key "format")
;;    "g"  '(nil :which-key "go")
;;    "gd" '(xref-find-definitions :which-key "find-definitions")
;;    "gr" '(xref-find-references :which-key "find-references")
;;    "R"  '(eglot-rename :which-key "rename")))

;; (add-hook 'c-mode-hook '+eglot/c-c++-config)
;; (add-hook 'c++-mode-hook '+eglot/c-c++-config)

(provide 'init-c)

;;; init-c.el ends here

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

;; Fixme: Recompile ccls, current ccls cant not found <iostream>
(use-package ccls
  :ensure t
  :requires init-lsp
  :commands lsp-ccls-enable
  :init
  (defun ccls//enable ()
    (condition-case nil
        (lsp-ccls-enable)
      (user-error nil)))
  ;; By default files ending in .h are treated as c files rather than c++ files.
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :hook ((c-mode c++-mode) . (lambda ()
                               (setq-local company-backends
                                           '((company-lsp :separate company-yasnippet)))
                               (ccls//enable)))
  :config
  (setq ccls-executable "/usr/bin/ccls")
  ;; Log file
  ;; (setq ccls-extra-args '("--log-file=/tmp/cq.log"))
  ;; Cache directory, both relative and absolute paths are supported
  ;; (setq ccls-cache-dir ".cquery_cached_index")
  ;; Initialization options
  (setq ccls-initialization-options
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))

(with-eval-after-load 'ccls
  (when (featurep 'evil)
    (evil-set-initial-state 'ccls-tree-mode 'emacs))
  (+funcs/set-leader-keys-for-major-mode
   '(c-mode-map c++-mode-map)
   "f"  '(lsp-format-buffer :which-key "format")
   "g"  '(nil :which-key "go")
   "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
   "gr" '(lsp-ui-peek-find-references :which-key "find-references")
   "gh" '(ccls-member-hierarchy :which-key "member-hierarchy")
   "R"  '(lsp-rename :which-key "rename")))

;; Experimental: c/c++-mode use ccls as default
;; (use-package eglot
;;   :ensure t
;;   :init (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;   :hook ((c-mode c++-mode) . eglot-ensure))

(use-package cmake-mode
  :ensure t)


(provide 'init-c)

;;; init-c.el ends here

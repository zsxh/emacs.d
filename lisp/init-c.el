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

(require 'init-language-server)

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
  (+c/set-leader-keys)
  (lsp)
  ;; (setq-local company-backends
  ;;             '((company-lsp :separate company-yasnippet)))
  )

(add-hook 'c-mode-hook '+c/lsp-ccls-config)
(add-hook 'c++-mode-hook '+c/lsp-ccls-config)

(use-package cmake-mode
  :ensure t)


(provide 'init-c)

;;; init-c.el ends here

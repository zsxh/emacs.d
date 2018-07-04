;; init-lsp.el --- lsp-mode Configurations	-*- lexical-binding: t -*-

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
;;  lsp-mode Configurations
;;

;;; Code:

(use-package lsp-mode
  :ensure t
  :config
  ;; (use-package lsp-imenu
  ;;   :ensure t
  ;;   :hook (lsp-after-open . lsp-enbale-imenu))

  (use-package company-lsp
    :after init-company
    :ensure t
    :config
    (push 'company-lsp company-backends)
    (setq company-lsp-enable-snippet t
          company-lsp-cache-candidates t))
  
  (use-package lsp-ui
    :ensure t
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)))


(provide 'init-lsp)

;;; init-lsp.el ends here

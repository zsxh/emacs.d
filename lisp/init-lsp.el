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

;; Lsp do not support temporary buffer yet
;; https://github.com/emacs-lsp/lsp-mode/issues/377
(use-package lsp-mode
  :ensure t
  :commands lsp-mode
  :init (setq lsp-inhibit-message t
              lsp-eldoc-render-all nil
              lsp-highlight-symbol-at-point nil)
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; Output the project root warning to *Messages* buffer
  (setq lsp-message-project-root-warning t))

(use-package company-lsp
  :after (company lsp-mode)
  :ensure t
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates nil
        company-transformers nil
        company-lsp-async t
        company-lsp-enable-recompletion t)
  ;; (push 'company-lsp company-backends)
  (push '(company-lsp :seperate company-yasnippet) company-backends))

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-ignore-duplicate t)
  (require 'lsp-ui-imenu)
  (lsp-ui-imenu-enable t)
  (require 'lsp-ui-sideline)
  (require 'lsp-ui-flycheck)
  (lsp-ui-sideline-mode)
  (lsp-ui-flycheck-enable t))

(provide 'init-lsp)

;;; init-lsp.el ends here

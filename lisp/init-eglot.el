;; init-eglot.el --- Emacs Client to Language Server Protocol	-*- lexical-binding: t -*-

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
;;  Eglot Settings
;;

;;; Code:

(use-package eglot
  :ensure t
  :hook ((sh-mode . eglot-ensure)
         ;; ((js-mode typescript-mode) . eglot-ensure)
         (go-mode . eglot-ensure)))

;; Experimental
;; TODO: Move to init-go.el
(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :commands go-mode)


(provide 'init-eglot)

;;; init-eglot.el ends here

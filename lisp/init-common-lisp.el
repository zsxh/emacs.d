;; init-common-lisp.el --- Common Lisp Configurations	-*- lexical-binding: t -*-

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
;;  Common Lisp Configurations
;;

;;; Code:

(use-package sly
  :ensure t
  :hook (lisp-mode . sly-mode)
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sly-macrostep
  :ensure t
  :commands macrostep-expand
  :bind (:map lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; TODO quicklisp


(provide 'init-common-lisp)

;;; init-common-lisp.el ends here

;; init-doc.el --- offline api doc	-*- lexical-binding: t -*-

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
;;  offline api doc
;;

;;; Code:

;; require `zeal' installation

;; zeal (dash for linux)
(use-package zeal-at-point
  :ensure t
  :commands zeal-at-point)

;; helm-dash
(use-package helm-dash
  :ensure t
  :commands (helm-dash helm-dash-at-point)
  :preface
  (setq helm-dash-docsets-path (expand-file-name "~/.local/share/Zeal/Zeal/docsets"))
  (setq helm-dash-browser-func 'eww)
  ;; (setq helm-dash-browser-func 'eaf-open-url)
  :config
  (setq helm-dash-common-docsets (helm-dash-installed-docsets)))


(provide 'init-doc)

;;; init-doc.el ends here

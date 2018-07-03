;; init-highlight.el --- Highlight EveryThing	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/.emacs.d

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
;;  Highlighting EveryThing
;;

;;; Code:

;; Highlight matching parenthesis
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Poor Performance
;; Highlight uncommitted changes
;; (use-package diff-hl
;;   :ensure t
;;   :hook ((after-init . global-diff-hl-mode)
;;          (dired-mode . diff-hl-dired-mode))
;;   :config
;;   (diff-hl-flydiff-mode)
;;   (with-eval-after-load 'magit
;;     (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
;;   ;; There's no fringe when Emacs is running in the console,
;;   ;; but the navigation and revert commands still work.
;;   ;; Consider turning diff-hl-margin-mode on, to show the indicators in the margin instead.
;;   (unless (display-graphic-p)
;;     (diff-hl-margin-mode))
;;   )

(provide 'init-highlight)

;;; init-highlight.el ends here

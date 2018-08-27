;; init-flycheck.el --- Flycheck Configuations	-*- lexical-binding: t -*-

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
;;  Flycheck Configuations
;;

;;; Code:

(use-package flycheck
  :ensure t
  :hook (prog-mode . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Display Flycheck errors in GUI tooltips
(if (display-graphic-p)
    (use-package flycheck-posframe
      :after flycheck
      :ensure t
      :hook (flycheck-mode . flycheck-posframe-mode))
  (use-package flycheck-popup-tip
    :after flycheck
    :ensure t
    :hook (flycheck-mode . flycheck-popup-tip-mode)))

;; Jump to and fix syntax errors via `avy'
(use-package avy-flycheck
  :after flycheck
  :ensure t
  :hook (flycheck-mode . avy-flycheck-setup))

(provide 'init-flycheck)

;;; init-flycheck.el ends here

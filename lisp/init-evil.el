;; init-evil.el --- Evil Configuations	-*- lexical-binding: t -*-

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
;; This program is distributed in the hope that it will be useful,;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, mA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Evil Configuations
;;

;;; Code:

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode)
  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)
  ;; 把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (with-eval-after-load 'dired
    (evil-collection-init 'dired)
    (evil-collection-define-key 'normal 'dired-mode-map
      (kbd "SPC") nil
      "," nil))
  (with-eval-after-load 'debug (evil-collection-init 'debug))
  (with-eval-after-load 'ibuffer (evil-collection-init 'ibuffer))
  (with-eval-after-load 'ediff (evil-collection-init 'ediff))
  (with-eval-after-load 'flycheck (evil-collection-init 'flycheck))
  (with-eval-after-load 'pdf-tools (require 'evil-collection-pdf) (evil-collection-pdf-setup))
  (with-eval-after-load 'neotree (evil-collection-init 'neotree)))


(provide 'init-evil)

;;; init-evil.el ends here

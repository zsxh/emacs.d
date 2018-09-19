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
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
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

;; Vim edit style
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  ;; remove all keybindings from insert-state keymap,it is VERY VERY important
  (setcdr evil-insert-state-map nil)
  ;; 把emacs模式下的按键绑定到Insert模式下
  (define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; message-buffer-mode config
  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-define-key 'normal messages-buffer-mode-map "q" 'quit-window)

  ;; compilation-mode config
  (defun +evil/compilation-mode-config ()
    (define-key compilation-mode-map "g" nil)
    (define-key compilation-mode-map "gr" 'recompile)
    (evil-set-initial-state 'compilation-mode 'normal)
    (evil-define-key 'normal compilation-mode-map "gr" 'recompile))
  (add-hook 'compilation-mode-hook #'+evil/compilation-mode-config))

;; Evil keybinding collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (with-eval-after-load 'package (evil-collection-init 'package-menu))
  (with-eval-after-load 'ibuffer (evil-collection-init 'ibuffer))
  (with-eval-after-load 'ediff (evil-collection-init 'ediff))
  (with-eval-after-load 'imenu-list (evil-collection-init 'imenu-list))
  (with-eval-after-load 'elfeed (evil-collection-init 'elfeed)))

;; press "%" to jump
;; usage: "%" jump between code block, tags
;;        "va%" evilmi-select-items
;;        "da%" evilmi-delete-items
;; All commands support numeric argument like “3%”, “5va%” or “9da%”
;;        “3%” will jump to a line 3 percentage down the file
(use-package evil-matchit
  :after evil
  :ensure t
  :config (global-evil-matchit-mode 1))

;; evil open/close/toggle folds rely on hideshow
;; "z a" evil-toggle-fold
;; "z m" evil-close-folds
;; "z r" evil-open-folds
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(provide 'init-evil)

;;; init-evil.el ends here

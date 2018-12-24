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
;; tips: press "%" in normal mode to jump between code block, tags
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

  ;; customize my vim normal state keybindings
  (define-key evil-normal-state-map (kbd "C-n") 'next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'previous-line)

  ;; message-buffer-mode config
  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-define-key 'normal messages-buffer-mode-map "q" 'quit-window)

  ;; compilation-mode config
  (defun +evil/compilation-mode-config ()
    (define-key compilation-mode-map "g" nil)
    (define-key compilation-mode-map "gr" 'recompile)
    (define-key compilation-mode-map "h" nil)
    (define-key compilation-mode-map "H" 'describe-mode)
    (evil-set-initial-state 'compilation-mode 'normal)
    (evil-define-key 'normal compilation-mode-map
      "gr" 'recompile
      "H"  'describe-mode)
    (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char))
  (add-hook 'compilation-mode-hook #'+evil/compilation-mode-config)

  ;; process-menu-mode config
  (defun +evil/process-menu-mode-config ()
    (evil-define-key 'normal process-menu-mode-map
      "S" 'tabulated-list-sort
      "d" 'process-menu-delete-process
      "g" 'revert-buffer
      "?" 'discribe-mode
      "q" 'quit-window))
  (add-hook 'process-menu-mode-hook #'+evil/process-menu-mode-config)

  ;; https://emacs.stackexchange.com/questions/31438/possible-not-to-use-undo-tree-in-evil-mode/34214#34214
  ;; https://github.com/emacs-evil/evil/issues/1074
  (setq undo-tree-enable-undo-in-region nil))

;; Evil keybinding collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (with-eval-after-load 'package (evil-collection-init 'package-menu))
  (with-eval-after-load 'ibuffer (evil-collection-init 'ibuffer))
  (with-eval-after-load 'ediff (evil-collection-init 'ediff))
  (with-eval-after-load 'imenu-list (evil-collection-init 'imenu-list)))


;; evil open/close/toggle folds rely on hideshow
;; "z a" evil-toggle-fold
;; "z m" evil-close-folds
;; "z r" evil-open-folds
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (define-key evil-normal-state-map (kbd "z f") 'hs-hide-level))

;; https://github.com/VanLaser/evil-nl-break-undo
;; It means that, for example, after you write an entire paragraph in insert state,
;; and then you hit u in normal state to undo, changes are undone line by line,
;; instead of the whole paragraph disappearing with one swift stroke.
(use-package evil-nl-break-undo
  :after evil
  :ensure t
  :hook ((text-mode prog-mode) . evil-nl-break-undo-mode))


(provide 'init-evil)

;;; init-evil.el ends here

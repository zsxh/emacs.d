;; init-edit.el --- Editor Configurations	-*- lexical-binding: t -*-

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
;;  Editor Configurations
;;

;;; Code:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq-default kill-whole-line t)           ; Kill line including '\n'

;; (setq-default major-mode 'text-mode)
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (turn-on-auto-fill)
;;             (diminish 'auto-fill-function)))

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   2
              tab-width        2
              indent-tabs-mode nil)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure t
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

  ;; :hook(;; show org ediffs unfolded
  ;;       (ediff-prepare-buffer . outline-show-all)
  ;;       ;; restore window layout when done
  ;;       (ediff-quit . winner-undo))
  ;; :config
  ;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; (setq ediff-split-window-function 'split-window-horizontally)
  ;; (setq ediff-merge-split-window-function 'split-window-horizontally))


;; automatic parenthesis pairing
(use-package elec-pair
  :ensure t
  :hook (after-init . electric-pair-mode))

;; Increase selected region by semantic units
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(setq mouse-drag-copy-region t)

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure t)

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-edit)

;;; init-edit.el ends here

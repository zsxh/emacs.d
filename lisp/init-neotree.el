;; init-neotree.el --- Neotree	-*- lexical-binding: t -*-

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
;;  Neotree
;;

;;; Code:

(use-package neotree
  :ensure t
  :init
  (setq neo-show-hidden-files t)
  :commands neotree-show
  :config
  (with-eval-after-load 'evil-collection
    ;; Evil-Keybindings
    (evil-collection-init 'neotree)
    ;; Custom Keybindings
    (require 'general)
    (general-define-key
     :states '(normal visual insert neotree-mode)
     :keymaps 'neotree-mode-map
     :major-modes t
     "h" '+neotree/neotree-collapse-or-up
     "l" '+neotree/neotree-expand-or-open
     "K" 'neotree-select-up-node
     "J" 'neotree-select-down-node))
  (with-eval-after-load 'winum
    ;; window 0 is reserved for file trees
    (add-to-list 'winum-assign-functions #'+neotree/winum-neotree-assign-func)))

;; Code from Spacemacs
(defun +neotree/neotree-expand-or-open (&optional arg)
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (if arg
            (neotree-enter arg)
          (let ((mru-winum (winum-get-number (get-mru-window))))
            (apply 'neotree-enter (list mru-winum))))))))

(defun +neotree/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun +neotree/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (+neotree/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun +neotree/winum-neotree-assign-func ()
  "Custom number assignment for neotree."
  (when (and (boundp 'neo-buffer-name)
             (string= (buffer-name) neo-buffer-name)
             ;; in case there are two neotree windows. Example: when
             ;; invoking a transient state from neotree window, the new
             ;; window will show neotree briefly before displaying the TS,
             ;; causing an error message. the error is eliminated by
             ;; assigning 0 only to the top-left window
             (eq (selected-window) (frame-first-window)))
    0))


(provide 'init-neotree)

;;; init-neotree.el ends here

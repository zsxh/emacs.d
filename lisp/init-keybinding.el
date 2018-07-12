;; init-keybinding.el --- KeyBindings	-*- lexical-binding: t -*-

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
;;  KeyBindings
;;

;;; Code:

(require 'init-funcs)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Main keybinding
(use-package general
  :after evil-collection
  :ensure t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode)
  (general-define-key
   ;; :states '(normal visual insert motion emacs)
   :states '(normal visual motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "/"   '(counsel-rg :which-key "ripgrep")
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "'"   '(ansi-term :which-key "open terminal")
   "0"   '(neotree-show :which-key "neotree")
   "1"   '(winum-select-window-1 :which-key "select-window-1")
   "2"   '(winum-select-window-2 :which-key "select-window-2")
   "3"   '(winum-select-window-3 :which-key "select-window-3")
   "4"   '(winum-select-window-4 :which-key "select-window-4")
   ;; Buffers
   "b"   '(nil :which-key "buffer")
   "bb"  '(ibuffer :which-key "buffers list")
   "bd"  '(kill-current-buffer :which-key "kill-current-buffer")
   "bs"  `(,(zsxh/switch-to-buffer-or-create "*scratch*") :which-key "*scratch*")
   "bm"  `(,(zsxh/switch-to-buffer-or-create "*Messages*") :which-key "*Messages*")
   "bn"  '(zsxh/new-empty-buffer :which-key "empty-buffer")
   ;; Error Flycheck
   "e"   '(nil :which-key "error")
   "el"  '(flycheck-list-errors :which-key "flycheck-list-errors")
   "ev"  '(flycheck-verify-setup :which-key "flycheck-verify-setup")
   ;; Files
   "f"   '(nil :which-key "file")
   "ff"  '(counsel-find-file :which-key "find files")
   ;; Help
   "h"   '(nil :which-key "help")
   "hd"  '(nil :which-key "details")
   "hdm" '((lambda () (interactive) (describe-variable 'major-mode)) :which-key "major-mode")
   "hdn" '((lambda () (interactive) (describe-variable 'minor-mode-list)) :which-key "minor-mode-list")
   "hp"  '(helpful-at-point :which-key "helpful-at-point")
   ;; Jump
   "j"   '(nil :which-key "jump")
   "jd"  '(dired-jump :which-key "dired-jump")
   ;; Org
   "o"   '(nil :which-key "org")
   "oa"  '(org-agenda :which-key "org-agenda")
   "oc"  '(org-capture :which-key "org-capture")
   ;; Window
   "w"   '(nil :which-key "window")
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wd"  '(delete-window :which-key "delete window")
   "wm"  '(delete-other-windows :which-key "max")
   ;; Toggle
   "t"   '(nil :which-key "toggle")
   "tl"  '(toggle-truncate-lines :which-key "truncate-lines")
   ;; Others
   "a"   '(nil :which-key "application")
   "c"   '(nil :which-key "comment")
   "cl"  '(comment-dwim-2 :which-key "comment line")
   "g"   '(nil :which-key "git")
   "gs"  '(magit :which-key "magit")))


(provide 'init-keybinding)

;;; init-keybinding.el ends here

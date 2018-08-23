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
  (which-key-setup-side-window-bottom)
  ;; Rename the entry, for 1 to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")) which-key-replacement-alist)
  ;; Hide other entries [2-9]
  (push '((nil . "select-window-[2-9]") . t) which-key-replacement-alist))

;; Global keybinding
(use-package general
  :after evil-collection
  :ensure t
  :config
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  (general-override-mode)
  (general-define-key
   :states '(normal visual motion emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   ;; misc
   "/"   '(counsel-rg :which-key "ripgrep")
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "last-buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "'"   '(shell-pop :which-key "shell-pop")
   "!"   '((lambda () (interactive) (call-interactively 'shell-command)) :which-key "shell-command")
   ":"   '((lambda () (interactive) (call-interactively 'eval-expression)) :which-key "eval-expression")
   ";"   '(comment-dwim-2 :which-key "comment-line")
   "0"   '(neotree-show :which-key "neotree")
   ;; winum-select-window
   "1"   'winum-select-window-1
   "2"   'winum-select-window-2
   "3"   'winum-select-window-3
   "4"   'winum-select-window-4
   "5"   'winum-select-window-5
   "6"   'winum-select-window-6
   "7"   'winum-select-window-7
   "8"   'winum-select-window-8
   "9"   'winum-select-window-9
   ;; Application
   "a"   '(nil :which-key "application")
   "ao"  '(nil :which-key "org")
   "aoa" '(org-agenda :which-key "org-agenda")
   "aoc" '(org-capture :which-key "org-capture")
   "ap"  '(list-processes :which-key "list-processes")
   "af"  '(describe-face :which-key "describe-face")
   ;; Buffers
   "b"   '(nil :which-key "buffer")
   "bb"  '(switch-to-buffer :which-key "switch-to-buffer")
   "bd"  '(kill-current-buffer :which-key "kill-current-buffer")
   "bi"  '(imenu-list-smart-toggle :which-key "imenu")
   "bI"  '(ibuffer :which-key "buffers list")
   "bm"  '((lambda () (interactive) (+funcs/switch-buffer-or-create "*Messages*")) :which-key "*Messages*")
   "bn"  '(+funcs/new-empty-buffer :which-key "empty-buffer")
   "bs"  '((lambda () (interactive) (+funcs/switch-buffer-or-create "*scratch*")) :which-key "*scratch*")
   ;; Error Flycheck
   "e"   '(nil :which-key "error")
   "el"  '(flycheck-list-errors :which-key "flycheck-list-errors")
   "ev"  '(flycheck-verify-setup :which-key "flycheck-verify-setup")
   ;; Files
   "f"   '(nil :which-key "file")
   "ff"  '(counsel-find-file :which-key "find-files")
   "fe"  '(+funcs/sudo-edit-current-file :which-key "sudo edit current file")
   ;; git
   "g"   '(nil :which-key "git")
   "gb"  '(magit-blame :which-key "magit-blame")
   "gc"  '(magit-blame-cycle-style :which-key "magit-blame-cycle-style")
   "gs"  '(magit :which-key "magit")
   ;; Help
   "h"   '(nil :which-key "help")
   "hd"  '(nil :which-key "details")
   "hdm" '((lambda () (interactive) (describe-variable 'major-mode)) :which-key "major-mode")
   "hdn" '((lambda () (interactive) (describe-variable 'minor-mode-list)) :which-key "minor-mode-list")
   "hf"  '(helpful-callable :which-key "helpful-callable")
   "hk"  '(helpful-key :which-key "helpful-key")
   "hv"  '(helpful-variable :which-key "helpful-variable")
   "hp"  '(helpful-at-point :which-key "helpful-at-point")
   ;; Jump
   "j"   '(nil :which-key "jump/goto")
   "jd"  '(dired-jump :which-key "dired-jump")
   "jc"  '(avy-goto-char :which-key "avy-goto-char")
   "jj"  '(avy-goto-char-in-line :which-key "avy-goto-char-in-line")
   "jl"  '(avy-goto-line :which-key "avy-goto-line")
   "jw"  '(avy-goto-word-1 :which-key "avy-goto-word-1")
   "je"  '(avy-goto-word-0 :which-key "avy-goto-word-0")
   ;; Text
   "t"   '(nil :which-key "text")
   "tm"  '(evil-multiedit-toggle-marker-here :which-key "multiedit-marker")
   "ts"  '(hydra-text-scale/body :which-key "scale")
   ;; Window
   "w"   '(nil :which-key "window")
   "w/"  '(split-window-right :which-key "split-right")
   "w-"  '(split-window-below :which-key "split-bottom")
   "wd"  '(delete-window :which-key "delete-window")
   "wm"  '(delete-other-windows :which-key "maximized")
   "ws"  '(hydra-window-scale/body :which-key "scale")
   "ww"  '(ace-swap-window :which-key "swap-window")
   ;; Toggle
   "T"   '(nil :which-key "toggle")
   "Tf"  '(font-lock-mode :which-key "syntax-highlighting")
   "Tl"  '(toggle-truncate-lines :which-key "truncate-lines")
   "Tn"  '(display-line-numbers-mode :which-key "display-line-numbers")
   "Tx"  '((lambda () (interactive) (shell-command "xmodmap ~/.Xmodmap")) :which-key "xmodmap")))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-text-scale (:hint nil)
    "zoom"
    ("k" text-scale-increase "text-scale-increase" :color pink)
    ("j" text-scale-decrease "text-scale-decrease" :color pink)
    ("q" nil "cancel" :color blue))
  (defhydra hydra-window-scale (:hint nil)
    "scale window"
    ("h" shrink-window-horizontally "shrink-window-horizontally" :color pink)
    ("l" enlarge-window-horizontally "enlarger-window-horizontally" :color pink)
    ("j" shrink-window "shrink-window" :color pink)
    ("k" enlarge-window "enlarge-window" :color pink)
    ("f" balance-windows "balance" :color pink)
    ("q" nil "cancel" :color blue)))

(provide 'init-keybinding)

;;; init-keybinding.el ends here

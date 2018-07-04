;; init-org.el --- Org Configuations	-*- lexical-binding: t -*-

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
;;  Org
;;

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :config
  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org")))

(use-package evil-org
  :ensure t
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Mode Keybindings
(with-eval-after-load 'evil-org
  (require 'init-keybinding)
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'org-mode-map
   :major-modes t
   :prefix "SPC"
   "m"   '(nil :which-key "major")
   "ma"  '(org-agenda :which-key "org-agenda")
   "mc"  '(org-capture :which-key "org-capture"))
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'org-mode-map
   :major-modes t
   :prefix ","
   "a"  '(org-agenda :which-key "org-agenda")
   "c"  '(org-capture :which-key "org-capture")))

(provide 'init-org)

;;; init-org.el ends here

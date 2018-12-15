;; init-emacs-enhancement.el --- enhance emacs	-*- lexical-binding: t -*-

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
;;  enhance emacs
;;

;;; Code:

;;;;;;;;;;;;;; *Help* ;;;;;;;;;;;;;;

;; A better *Help* buffer
(use-package helpful
  :ensure t
  :defines ivy-initial-inputs-alist
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . helpful-callable) ;; replace built-in `describe-function'
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable))
  :config
  (with-eval-after-load 'ivy
    (dolist (cmd '(helpful-callable
                   helpful-variable
                   helpful-function
                   helpful-macro
                   helpful-command))
      (cl-pushnew `(,cmd . "^") ivy-initial-inputs-alist)))
  (with-eval-after-load 'evil
    (evil-define-key 'normal helpful-mode-map "q" 'quit-window)))

;;;;;;;;;;;;;; *Buffer* ;;;;;;;;;;;;;;

(use-package ibuffer-vc
  :ensure t
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer . (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

;;;;;;;;;;;;;; Dired ;;;;;;;;;;;;;;

;; Dired Configs
(use-package dired
  :ensure nil
  :init
  ;; dired "human-readable" format
  (setq dired-listing-switches "-alh --time-style=long-iso --group-directories-first")
  :config
  ;; Customize dired-directory foreground color
  (set-face-foreground 'dired-directory "#3B6EA8")

  ;;narrow dired to match filter
  (use-package dired-narrow
    :ensure t
    :commands dired-narrow)

  (with-eval-after-load 'evil-collection
    (evil-collection-init 'dired)
    (evil-define-key 'normal dired-mode-map
      (kbd "SPC") nil
      "," nil))
  (+funcs/set-leader-keys-for-major-mode
   dired-mode-map
   "/" '(dired-narrow :which-key "dired-narrow")
   "r" '(dired-narrow-regexp :which-key "dired-narrow-regexp"))

  ;; Editable Dired mode configs
  (with-eval-after-load 'wdired
    (+funcs/set-leader-keys-for-major-mode
     wdired-mode-map
     "c" '(wdired-finish-edit :which-key "finish edit")
     "k" '(wdired-abort-changes :which-key "abort changes")
     "q" '(wdired-exit :which-key "exit"))
    (when all-the-icons-dired-mode
      (advice-add #'wdired-change-to-wdired-mode :before (lambda () (all-the-icons-dired-mode -1)))
      (advice-add #'wdired-change-to-dired-mode :after (lambda () (all-the-icons-dired-mode))))))


(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here

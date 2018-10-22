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
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Custom flycheck fringe icons
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111
            #b00111111))
  (let ((bitmap 'my-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-error
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-warning
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-info
      :fringe-face 'flycheck-fringe-info))

  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)

  (with-eval-after-load 'evil-collection
    (evil-collection-init 'flycheck)
    (evil-define-key 'normal flycheck-error-list-mode-map
      "j" 'flycheck-error-list-next-error
      "k" 'flycheck-error-list-previous-error)))

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

;; toggle flycheck window
(defun +flycheck/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun +flycheck/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

(defun +flycheck/popup-errors ()
  "Show the error list for the current buffer."
  (interactive)
  (unless flycheck-mode
    (user-error "Flycheck mode not enabled"))
  ;; Create and initialize the error list
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (flycheck-error-list-mode)))
  (flycheck-error-list-set-source (current-buffer))
  ;; Reset the error filter
  (flycheck-error-list-reset-filter)
  ;; Popup the error list in the bottom window
  (popwin:popup-buffer flycheck-error-list-buffer)
  ;; Finally, refresh the error list to show the most recent errors
  (flycheck-error-list-refresh))

;; Jump to and fix syntax errors via `avy'
;; (use-package avy-flycheck
;;   :after flycheck
;;   :ensure t
;;   :hook (flycheck-mode . avy-flycheck-setup))

(provide 'init-flycheck)

;;; init-flycheck.el ends here

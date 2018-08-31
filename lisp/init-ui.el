;; init-ui.el --- Emacs UI, Startup Screen Settings	-*- lexical-binding: t -*-

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
;;  Emacs UI, Startup Screen Settings
;;

;;; Code:

;; Minimal UI in init.el when emacs-version < 27
(when (version< emacs-version "27")
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1))

;; Startup windows size
;; (toggle-frame-maximized)
(toggle-frame-fullscreen)

;; Theme
(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (setq doom-neotree-file-icons t)
  (doom-themes-neotree-config) ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

;; Display time on modeline
(setq display-time-format "%Y/%m/%d 周%a %H:%M")
(setq display-time-default-load-average nil) ; don't show load avera
(display-time-mode)

;; Font
;; (set-frame-font "SF Mono 13" nil t)
(set-frame-font "SF Mono-13.5:weight=semi-bold" nil t)

;; org table font
(custom-set-faces
 '(org-table ((t (:family "Ubuntu Mono derivative Powerline")))))

;; Line Number
;; (use-package display-line-numbers
;;   :ensure nil
;;   :hook (prog-mode . display-line-numbers-mode))

;; Emacs startup *scratch* buffer
(setq initial-buffer-choice t)

;; Numbered window shortcuts
(use-package winum
  :ensure t
  :hook (after-init . winum-mode))

(use-package imenu-list
  :defer t
  :ensure t
  :init
  (progn
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)))

(use-package ace-window
  :ensure t
  :commands (ace-swap-window))

(provide 'init-ui)

;;; init-ui.el ends here

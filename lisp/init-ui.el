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

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Startup windows size
(toggle-frame-maximized)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; Font
(set-frame-font "SF Mono 13" nil t)

;; org table font
(custom-set-faces
 '(org-table ((t (:family "Ubuntu Mono derivative Powerline"))))
 )

;; Line Number
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; Emacs startup *scratch* buffer
(setq initial-buffer-choice t)

;; Numbered window shortcuts
(use-package winum
  :ensure t
  :hook (after-init . winum-mode))


(provide 'init-ui)

;;; init-ui.el ends here

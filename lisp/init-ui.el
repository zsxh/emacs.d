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

(eval-when-compile
  (require 'init-custom))

;; Minimal UI in init.el when emacs-version < 27
(when (version< emacs-version "27")
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1))

;; Startup frame size
(if (string= personal-frame-startup-size "max")
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; Theme
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8")
  (use-package font-lock+ :quelpa (font-lock+ :fetcher wiki)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme (intern personal-doom-theme) t)

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
  :preface
  (defun my-doomline-init ()
    (doom-modeline-init)
    (with-current-buffer "*Messages*"
      (doom-modeline-set-modeline 'main)))
  :hook (after-init . my-doomline-init)
  :config
  (setq doom-modeline-major-mode-icon nil))

;; Display time on modeline
(setq display-time-format "%Y/%m/%d 周%a %H:%M")
(setq display-time-default-load-average nil) ; don't show load avera
(display-time-mode)

;; Frame font
;; (set-frame-font "SF Mono 13" nil t)
(set-frame-font "SF Mono-13.5:weight=semi-bold" nil t)

;; Line Number
;; (use-package display-line-numbers
;;   :ensure nil
;;   :hook (prog-mode . display-line-numbers-mode))

;; Emacs startup *scratch* buffer
(setq initial-buffer-choice t)


(provide 'init-ui)

;;; init-ui.el ends here

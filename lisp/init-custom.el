;; init-custom.el --- Customizations	-*- lexical-binding: t -*-

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
;;  Customizations
;;

;;; Code:

(defgroup personal nil
  "Personal Emacs customizations."
  :group 'convenience)

(defcustom personal-eaf-grip-token nil
  "Github personal access token for eaf-markdown-previewer.
https://github.com/manateelazycat/emacs-application-framework#markdown-previewer"
  :type 'string)

(defcustom personal-doom-theme "doom-vibrant"
  "Customize doom-themes such as `\"doom-vibrant\"' `\"doom-nord-light\"'.
Origin repo: https://github.com/hlissner/emacs-doom-themes"
  :type 'string)

(defcustom personal-elfeed-feeds nil
  "Rss feeds, eg: ((\"https://oremacs.com/atom.xml\" oremacs))."
  :type 'cons)

(defcustom personal-frame-startup-size "max"
  "Startup frame size. `\"max\"' means maximized frame and `\"fullscreen\"' means fullscreen frame."
  :type 'string)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file))


(provide 'init-custom)

;;; init-custom.el ends here

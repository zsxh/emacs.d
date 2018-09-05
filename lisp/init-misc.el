;; init-misc.el --- Some other things	 -*- lexical-binding: t -*-

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
;;  Some other things
;;

;;; Code:


;; rss reader
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-feeds
        '(("http://www.norvig.com/rss-feed.xml" norvig)
          ("https://oremacs.com/atom.xml" oremacs)
          ("http://blog.binchen.org/rss.xml" chenbin)
          ("http://www.planet.emacsen.org/atom.xml" Planet Emacsen)
          ("https://emacs-china.org/latest.rss" emacs china)
          ("https://emacs-china.org/post.rss" emacs china))))


(provide 'init-misc)

;;; init-misc.el ends here

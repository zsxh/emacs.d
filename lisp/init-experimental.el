;; init-experimental.el --- Experimental Feature	-*- lexical-binding: t -*-

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
;;  Experimental Feature
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(let ((default-directory (expand-file-name "experimental" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; https://github.com/manateelazycat/emacs-application-framework
(use-package eaf
  :commands (eaf-open eaf-open-url)
  :config
  (when personal-eaf-grip-token
    (setq eaf-grip-token personal-eaf-grip-token)))

;; Pdf viewer settings
(add-to-list 'auto-mode-alist
             '("\\.pdf\\'" . (lambda ()
                               (let ((filename buffer-file-name))
                                 (eaf-open filename)
                                 (kill-buffer (file-name-nondirectory filename))))))

;; Markdowm Previewer
(with-eval-after-load 'markdown-mode
  (defun eaf-markdown-previewer ()
    "Markdown Previewer."
    (interactive)
    (eaf-open buffer-file-name))
  (+funcs/try-general-major-key markdown-mode-map
                                "p" '(eaf-markdown-previewer :which-key "previewer")))

;; Better eshell
;; https://github.com/manateelazycat/aweshell
;; (use-package aweshell
;;   :commands aweshell-new
;;   :quelpa ((aweshell :fetcher github :repo "manateelazycat/aweshell")))

;; This extension will ask me Chinese words and then insert translation as variable or function name.
;; https://github.com/manateelazycat/insert-translated-name
(use-package insert-translated-name
  :commands insert-translated-name-insert
  :quelpa ((insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name")))


(provide 'init-experimental)

;;; init-experimental.el ends here

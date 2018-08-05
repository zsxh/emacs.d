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
  :defer 1
  :ensure org-plus-contrib
  :config
  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org")))

(use-package evil-org
  :after (org evil)
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; custom keybindings
  (evil-define-key 'motion org-agenda-mode-map "?" 'org-agenda-view-mode-dispatch)
  ;; Open links and files with RET in normal state
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point))

;; Mode Keybindings
(with-eval-after-load 'evil-org
  (require 'general)
  (zsxh/define-major-key org-mode-map
                         "a"  '(org-agenda :which-key "org-agenda")
                         "c"  '(org-capture :which-key "org-capture")))

;; Org for blog
(use-package org-page
  :defer 1
  :ensure t
  :config
  (setq op/repository-directory "~/org/zsxh.github.io")
  (setq op/site-domain "https://zsxh.github.io/")
  (setq op/personal-disqus-shortname "zsxhspace")
  (setq op/site-main-title "Hello World的一千种写法")

  (setq op/repository-org-branch "source")  ;; default is "source"
  (setq op/repository-html-branch "master") ;; default is "master"

  (setq op/personal-github-link "https://github.com/zsxh")

  (setq op/personal-google-analytics-id "UA-119871562-1")

  ;; (setq op/highlight-render 'htmlize)
  (setq op/theme 'mdo))


(provide 'init-org)

;;; init-org.el ends here

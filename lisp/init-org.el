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
  :mode ("\\.org\\'" . org-mode)
  :commands org-open-at-point
  :config
  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org")))

;; Org-mode keybindings
(use-package evil-org
  :after (org evil)
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)
              ;; Open links and files with RET in normal state
              (evil-define-key 'normal org-mode-map
                (kbd "RET") 'org-open-at-point)))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "?" 'org-agenda-view-mode-dispatch
    "0" 'digit-argument)

  (evil-define-minor-mode-key 'normal 'org-src-mode
    ",c" 'org-edit-src-exit
    ",k" 'org-edit-src-abort)

  (evil-define-minor-mode-key 'normal 'org-capture-mode
    ",c" 'org-capture-finalize
    ",k" 'org-capture-kill
    ",w" 'org-capture-refile)

  ;; major mode keybindings
  (+funcs/try-general-major-key org-mode-map
                                "a"  '(org-agenda :which-key "agenda")
                                "c"  '(nil :which-key "capture/clock")
                                "cc" '(org-capture :which-key "capture")
                                "ci" '(org-clock-in :which-key "clock-in")
                                "co" '(org-clock-out :which-key "clock-out")
                                "cr" '(org-clock-report :which-key "clock-report")
                                "t"  '(nil :which-key "toggle")
                                "ti" '(org-toggle-inline-images :which-key "toggle-inline-images")
                                "tl" '(org-toggle-link-display :which-key "toggle-link-display")
                                "'"  '(org-edit-special :which-key "editor")))

;; Org for blog
(use-package org-page
  :after org
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

;; ========================
;; Org Bable Configuations
;; ========================

;; An extension to restclient.el for emacs that provides org-babel support
(when (package-installed-p 'restclient)
  (use-package ob-restclient
    :after org
    :ensure t
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)))))

;; ob-async enables asynchronous execution of org-babel src blocks
(use-package ob-async
  :after org
  :ensure t
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda ()
               (setq inferior-julia-program-name "julia")))
  ;; ob-python define their own :async keyword that conflicts with ob-async
  (setq ob-async-no-async-languages-alist '("ipython")))

;; https://github.com/gregsexton/ob-ipython
(use-package ob-ipython
  :after org
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t))))


(provide 'init-org)

;;; init-org.el ends here

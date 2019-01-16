;; init-org.el --- Org Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Org
;;

;;; Code:

;; https://orgmode.org/Changes.html
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-c C-," . org-insert-structure-template))
  :commands org-open-at-point
  :config
  (setq org-confirm-babel-evaluate nil) ;don't prompt me to confirm everytime I want to evaluate a block

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org"))

  ;; Org table font
  (custom-set-faces
   '(org-table ((t (:family "Ubuntu Mono derivative Powerline")))))

  ;; Org block face
  (set-face-background 'org-block "#E0E0E0")
  (set-face-background 'org-quote nil)
  (set-face-background 'org-block-begin-line nil)
  (set-face-background 'org-block-end-line nil)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; enable ob-*lang* yourself

  ;; ob-sh renamed to ob-shell since 26.1.
  (when (>= emacs-major-version 26)
    (require 'ob-shell))

  (if (>= emacs-major-version 26)
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :ensure t
    :after org
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :ensure t
    :after org
    :init (cl-pushnew '(rust . t) load-language-list))

  ;; https://github.com/gregsexton/ob-ipython
  ;; enable ob-ipython and ob-python
  (use-package ob-ipython
    :ensure t
    :after org
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  ;; An extension to restclient.el for emacs that provides org-babel support
  (when (package-installed-p 'restclient)
    (use-package ob-restclient
      :ensure t
      :after org
      :init (cl-pushnew '(restclient . t) load-language-list)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))

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

  (with-eval-after-load 'org-src
    (evil-define-minor-mode-key 'normal 'org-src-mode
      ",c" 'org-edit-src-exit
      ",k" 'org-edit-src-abort))

  (with-eval-after-load 'org-capture
    (evil-define-minor-mode-key 'normal 'org-capture-mode
      ",c" 'org-capture-finalize
      ",k" 'org-capture-kill
      ",w" 'org-capture-refile))

  ;; major mode keybindings
  (+funcs/set-leader-keys-for-major-mode
   'org-mode-map
   "a"  '(org-agenda :which-key "agenda")
   "c"  '(nil :which-key "capture/clock")
   "cc" '(org-capture :which-key "capture")
   "ci" '(org-clock-in :which-key "clock-in")
   "co" '(org-clock-out :which-key "clock-out")
   "cr" '(org-clock-report :which-key "clock-report")
   "i"  '(nil :which-key "insert")
   "is" '(org-insert-structure-template :which-key "structure-template")
   "it" '(org-time-stamp :which-key "time-stamp")
   "T"  '(nil :which-key "toggle")
   "Ti" '(org-toggle-inline-images :which-key "toggle-inline-images")
   "Tl" '(org-toggle-link-display :which-key "toggle-link-display")
   "'"  '(org-edit-special :which-key "editor")))

;; Org for blog
(use-package org-page
  :after org
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


(provide 'init-org)

;;; init-org.el ends here

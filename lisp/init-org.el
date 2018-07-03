;; -*- lexical-binding: t -*-

(use-package org
  :ensure org-plus-contrib
  :config
  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org")))

(use-package evil-org
  :ensure t
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Mode Keybindings
(with-eval-after-load 'evil-org
  (require 'init-which-key)
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'org-mode-map
   :major-modes t
   :prefix "SPC"
   "m"   '(nil :which-key "major")
   "ma"  '(org-agenda :which-key "org-agenda")
   "mc"  '(org-capture :which-key "org-capture"))
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'org-mode-map
   :major-modes t
   :prefix ","
   "a"  '(org-agenda :which-key "org-agenda")
   "c"  '(org-capture :which-key "org-capture")))


(provide 'init-org)

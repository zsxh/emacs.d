;; -*- lexical-binding: t -*-

(require 'init-funcs)


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Main keybinding
(use-package general
  :ensure t
  :config (general-define-key
           :states '(normal visual insert emacs)
           :keymaps 'override
           :prefix "SPC"
           :non-normal-prefix "M-SPC"
           "/"   '(counsel-rg :which-key "ripgrep")
           "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
           "SPC" '(counsel-M-x :which-key "M-x")
           "'"   '(ansi-term :which-key "open terminal")
           "1"   '(select-window-1 :which-key "select-window-1")
           "2"   '(select-window-2 :which-key "select-window-2")
           "3"   '(select-window-3 :which-key "select-window-3")
           "4"   '(select-window-4 :which-key "select-window-4")
           ;; Buffers
           "b"   '(nil :which-key "buffer")
           "bb"  '(ibuffer :which-key "buffers list")
           "bd"  '(kill-current-buffer :which-key "kill-current-buffer")
           "bs"  '(switch-buffer-scratch :which-key "switch to *scratch*")
           "bn"  '(new-empty-buffer :which-key "empty-buffer")
           ;; Error Flycheck
           "e"   '(nil :which-key "error")
           "el"  '(flycheck-list-errors :which-key "flycheck-list-errors")
           "ev"  '(flycheck-verify-setup :which-key "flycheck-verify-setup")
           ;; Files
           "f"   '(nil :which-key "file")
           "ff"  '(counsel-find-file :which-key "find files")
           ;; Jump
           "j"   '(nil :which-key "jump")
           "jd"  '(dired-jump :which-key "dired-jump")
           ;; Org
           "o"   '(nil :which-key "org")
           "oa"  '(org-agenda :which-key "org-agenda")
           "oc"  '(org-capture :which-key "org-capture")
           ;; Window
           "w"   '(nil :which-key "window")
           "wl"  '(windmove-right :which-key "move right")
           "wh"  '(windmove-left :which-key "move left")
           "wk"  '(windmove-up :which-key "move up")
           "wj"  '(windmove-down :which-key "move bottom")
           "w/"  '(split-window-right :which-key "split right")
           "w-"  '(split-window-below :which-key "split bottom")
           "wd"  '(delete-window :which-key "delete window")
           "wm"  '(delete-other-windows :which-key "max")
           ;; Toggle
           "t"   '(nil :which-key "toggle")
           ;; Others
           "a"   '(nil :which-key "application")
           "c"   '(nil :which-key "comment")
           "cl"  '(comment-dwim-2 :which-key "comment line")
           "g"   '(nil :which-key "git")
           "gs"  '(magit :which-key "magit")
           ))

(provide 'init-which-key)

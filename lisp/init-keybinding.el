;; init-keybinding.el --- KeyBindings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  KeyBindings
;;

;;; Code:

(require 'init-funcs)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; Rename the entry, for 1 to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")) which-key-replacement-alist)
  ;; Hide other entries [2-9]
  (push '((nil . "select-window-[2-9]") . t) which-key-replacement-alist))

;; keybindings with which-key,evil well supported
(use-package general
  :ensure t
  :config
  ;; (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  ;; (general-override-mode)
  (general-define-key
   :states '(normal visual motion emacs)
   :keymaps '(override global)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "last-buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "'"   '(shell-pop :which-key "shell-pop")
   "!"   '(shell-command :which-key "shell-command")
   ":"   '(eval-expression :which-key "eval-expression")
   ";"   '(comment-dwim-2 :which-key "comment-line")
   "0"   '(neotree-show :which-key "neotree")
   "/"   '(color-rg-search-input :which-key "rg-current-dir")
   ;; winum-select-window
   "1"   'winum-select-window-1
   "2"   'winum-select-window-2
   "3"   'winum-select-window-3
   "4"   'winum-select-window-4
   "5"   'winum-select-window-5
   "6"   'winum-select-window-6
   "7"   'winum-select-window-7
   "8"   'winum-select-window-8
   "9"   'winum-select-window-9
   ;; Application
   "a"   '(nil :which-key "application")
   "af"  '(elfeed :which-key "elfeed")
   "ao"  '(nil :which-key "org")
   "aoa" '(org-agenda :which-key "org-agenda")
   "aoc" '(org-capture :which-key "org-capture")
   "ap"  '(list-processes :which-key "list-processes")
   "ar"  '(+web/restclient-new-buffer :which-key "restclient")
   "ay"  '(nil :which-key "youdao")
   "ays" '(youdao-dictionary-search-from-input :which-key "search-from-input")
   "ayy" '(youdao-dictionary-search-at-point-tooltip :which-key "translate-at-point")
   "ayv" '(youdao-dictionary-play-voice-at-point :which-key "voice-at-point")
   "aw"  '(nil :which-key "emacs-web-wowser")
   "aww" '(eww :which-key "open eww")
   "awb" '(eww-list-bookmarks :which-key "list-bookmarks")
   ;; Buffers
   "b"   '(nil :which-key "buffer")
   "bb"  '(switch-to-buffer :which-key "switch-to-buffer")
   "bd"  '(kill-current-buffer :which-key "kill-current-buffer")
   "bi"  '(imenu-list-smart-toggle :which-key "imenu")
   "bI"  '(ibuffer :which-key "buffers list")
   "bm"  '((lambda () (interactive) (switch-to-buffer (messages-buffer))) :which-key "*Messages*")
   "bn"  '((lambda () (interactive) (+funcs/switch-empty-buffer-or-create "untitled")) :which-key "empty-buffer")
   "bN"  '(+funcs/new-empty-buffer :which-key "empty-buffer")
   "bs"  '((lambda () (interactive) (+funcs/switch-buffer-or-create "*scratch*")) :which-key "*scratch*")
   ;; Doc
   "d"   '(nil :which-key "doc")
   "dd"  '(zeal-at-point :which-key "zeal-at-point")
   "dh"  '(helm-dash-at-point :which-key "helm-dash-at-point")
   "dH"  '(helm-dash :which-key "helm-dash")
   ;; Files
   "f"   '(nil :which-key "file")
   "ff"  '(counsel-find-file :which-key "find-files")
   "fe"  '(+funcs/sudo-edit-current-file :which-key "sudo-edit-current-file")
   "fp"  '(xah-copy-file-path :which-key "copy-file-path")
   "fs"  '(find-name-dired :which-key "search-files")
   ;; Git
   "g"   '(nil :which-key "git")
   "gb"  '(magit-blame :which-key "magit-blame")
   "gc"  '(magit-blame-cycle-style :which-key "magit-blame-cycle-style")
   "gf"  '(magit-file-popup :which-key "magit-file-popup")
   "gs"  '(magit :which-key "magit")
   ;; Help
   "h"   '(nil :which-key "help")
   "hc"  '(company-diag :which-key "company-diag")
   "hd"  '(nil :which-key "details")
   "hdm" '((lambda () (interactive) (describe-variable 'major-mode)) :which-key "major-mode")
   "hdn" '((lambda () (interactive) (describe-variable 'minor-mode-list)) :which-key "minor-mode-list")
   "hf"  '(helpful-callable :which-key "helpful-callable")
   "hF"  '(describe-face :which-key "describe-face")
   "hk"  '(helpful-key :which-key "helpful-key")
   "hS"  '(hydra-emacs-cheatsheet/body :which-key "my-emacs-cheatsheet")
   "hv"  '(helpful-variable :which-key "helpful-variable")
   "hp"  '(helpful-at-point :which-key "helpful-at-point")
   "hu"  '(upgrade-packages :which-key "upgrade-packages")
   "hw"  '(nil :which-key "which-key")
   "hwk" '(which-key-show-top-level :which-key "show-top-level")
   ;; Jump
   "j"   '(nil :which-key "jump/goto")
   "jc"  '(avy-goto-char :which-key "avy-goto-char")
   "jd"  '(dired-jump :which-key "dired-jump")
   "jD"  '(dired-jump-other-window :which-key "dired-jump-other-window")
   "je"  '(avy-goto-word-0 :which-key "avy-goto-word-0")
   "jf"  '(find-file-at-point :which-key "find-file-at-point")
   "jj"  '(avy-goto-char-in-line :which-key "avy-goto-char-in-line")
   "jl"  '(avy-goto-line :which-key "avy-goto-line")
   "jw"  '(avy-goto-word-1 :which-key "avy-goto-word-1")
   ;; Project
   "p"   '(nil :which-key "project")
   "p/"  '(color-rg-search-project :which-key "rg-search-project")
   "p'"  '(+shell/projectile-shell-pop :which-key "project-root-shell-pop")
   "pb"  '(projectile-switch-to-buffer :which-key "switch-to-project-buffer")
   "pd"  '(projectile-dired :which-key "project-root-dired")
   "pf"  '(project-find-file :which-key "project-find-file")
   "pg"  '(counsel-git :which-key "counsel-git")
   "pp"  '(projectile-switch-project :which-key "switch-project")
   "pt"  '(+neotree/find-project-root :which-key "neotree-find-project-root")
   ;; Text
   "t"   '(nil :which-key "text")
   "tb"  '(xah-select-block :which-key "select-block")
   "tB"  '(comment-box :which-key "comment-box")
   "td"  '(xah-delete-current-text-block :which-key "delete-block")
   "ti"  '(nil :which-key "insert")
   "tit" '(insert-translated-name-insert :which-key "chinese->engish")
   "tm"  '(evil-multiedit-toggle-marker-here :which-key "multiedit-marker")
   "ts"  '(hydra-text-scale/body :which-key "scale")
   "tS"  '(hydra-string-inflection/body :which-key "string-inflection-cycle")
   ;; "tt"  '(tiny-expand :which-key "tiny-expand")
   "tw"  '(+funcs/shrink-whitespaces :which-key "shrink-whitespace")
   ;; View
   "v"   '(nil :which-key "view")
   "vn"  '(ivy-push-view :which-key "ivy-push-view")
   "vd"  '(ivy-pop-view :which-key "ivy-pop-view")
   "vv"  '(ivy-switch-view :which-key "ivy-switch-view")
   ;; Window
   "w"   '(nil :which-key "window")
   "w/"  '(split-window-right :which-key "split-right")
   "w-"  '(split-window-below :which-key "split-bottom")
   "wb"  '(balance-windows :which-key "balance-windows")
   "wd"  '(delete-window :which-key "delete-window")
   "wm"  '(delete-other-windows :which-key "maximized")
   "ws"  '(hydra-window-scale/body :which-key "scale")
   "ww"  '(ace-swap-window :which-key "swap-window")
   ;; Toggle
   "T"   '(nil :which-key "toggle")
   "Td"  '(toggle-debug-on-error :which-key "debug-on-error")
   "Tf"  '(font-lock-mode :which-key "syntax-highlighting")
   "Tl"  '(toggle-truncate-lines :which-key "truncate-lines")
   "Tn"  '(display-line-numbers-mode :which-key "display-line-numbers")
   "Tp"  '(toggle-socks-proxy :which-key "socks-proxy")
   "Tx"  '((lambda () (interactive) (shell-command "xmodmap ~/.Xmodmap")) :which-key "xmodmap")
   "Tz"  '(evil-toggle-fold :which-key "evil-toggle-fold")))

(use-package hydra :ensure t)

(defhydra hydra-text-scale (:hint nil)
  "zoom"
  ("k" text-scale-increase "text-scale-increase" :color pink)
  ("j" text-scale-decrease "text-scale-decrease" :color pink)
  ("q" nil "quit" :color blue))

(defhydra hydra-window-scale (:hint nil)
  "scale window"
  ("h" shrink-window-horizontally "shrink-window-horizontally" :color pink)
  ("l" enlarge-window-horizontally "enlarger-window-horizontally" :color pink)
  ("j" shrink-window "shrink-window" :color pink)
  ("k" enlarge-window "enlarge-window" :color pink)
  ("b" balance-windows "balance" :color pink)
  ("q" nil "quit" :color blue))

(defhydra hydra-emacs-cheatsheet (:color pink :hint nil :exit t)
  "
    ^Emacs Cheatsheet^
    ^^^^^----------------------
    _s_: \"C-x TAB\" indent-rigidly
    "
  ("s" indent-rigidly)                  ; shift line[s] left/right
  ("q" nil "quit"))

(defhydra hydra-string-inflection (:hint nil)
  "cycle text objects through camelCase, kebab-case, snake case and UPPER CASE.
"
  ("s" string-inflection-all-cycle "string-inflection-all-cycle" :color pink)
  ("q" nil "quit" :color blue))


(provide 'init-keybinding)

;;; init-keybinding.el ends here

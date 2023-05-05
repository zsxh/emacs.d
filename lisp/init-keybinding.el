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
  :init
  (setq which-key-popup-type 'side-window
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 1
        which-key-side-window-slot -10)
  :config
  ;; Rename the entry, for 1 to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")) which-key-replacement-alist)
  ;; Hide other entries [2-9]
  (push '((nil . "select-window-[2-9]") . t) which-key-replacement-alist)
  (setq resize-mini-windows t)
  (which-key-mode))

;; NOTE: `Master-Key-Bindings-Emacs' https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; NOTE: `general.el' https://github.com/noctuid/general.el

;; keybindings with which-key,evil well supported
(use-package general)

(with-eval-after-load 'evil
  (define-key global-map "\M-o" nil) ; disable `facemenu-keymap' default keybindings first
  ;; NOTE: https://github.com/noctuid/evil-guide#leader-key
  (general-define-key
   :states '(normal visual motion insert emacs)
   :keymaps '(override global)
   :prefix "SPC" ;`:prefix' will only apply to evil states not listed in `general-non-normal-states'
   :global-prefix "M-o"
   "" nil
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "last-buffer")
   "SPC" '(execute-extended-command :which-key "M-x")
   "'" '(separedit :which-key "edit-comment")
   "!" '(shell-command :which-key "shell-command")
   ":" '(eval-expression :which-key "eval-expression")
   ";" '(comment-dwim-2 :which-key "comment-line")
   ;; "0" '(neotree-show :which-key "neotree")
   "0" '(dirvish-side :which-key "dirvish-side")
   "/" '(rg :which-key "ripgrep")
   ;; winum-select-window
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9
   ;; Application
   "a" '(nil :which-key "application")
   "ac" '(nil :which-key "client")
   "acr" '(+web/restclient-new-buffer :which-key "restclient")
   "aC" '(cfw:open-org-calendar :which-key "calendar")
   "ad" '(docker :which-key "docker")
   "af" '(elfeed :which-key "elfeed")
   "am" '(man :which-key "man-page")
   "ao" '(nil :which-key "org")
   "aoa" '(org-agenda :which-key "org-agenda")
   "aoc" '(org-capture :which-key "org-capture")
   "ap" '(list-processes :which-key "list-processes")
   "aR" '(re-builder :which-key "re-builder")
   "at" '(telega :which-key "telega")
   "ay" '(nil :which-key "dictionary")
   "ays" '(youdao-dictionary-search-from-input :which-key "search-from-input")
   "ayy" '(youdao-dictionary-search-at-point-posframe :which-key "translate-at-point")
   "ayv" '(youdao-dictionary-play-voice-at-point :which-key "voice-at-point")
   ;; Buffers
   "b" '(nil :which-key "buffer")
   "bb" '(+funcs/switch-to-buffer-dwim :which-key "switch-to-buffer-dwim")
   "bB" '(switch-to-buffer :which-key "switch-to-buffer")
   "bd" '(kill-current-buffer :which-key "kill-current-buffer")
   "bi" '(imenu-list-smart-toggle :which-key "imenu")
   "bI" '(ibuffer :which-key "buffers list")
   "bm" '((lambda () (interactive) (switch-to-buffer (messages-buffer))) :which-key "*Messages*")
   "bn" '((lambda () (interactive) (+funcs/switch-empty-buffer-or-create "untitled")) :which-key "jump-empty-buffer")
   "bN" '(+funcs/new-empty-buffer :which-key "create-empty-buffer")
   "bs" '(scratch-buffer :which-key "*scratch*")
   ;; Files
   "f" '(nil :which-key "file")
   "fd" '(+dired/find-program :which-key "find-files-in-dired")
   "fe" '(+funcs/sudo-edit-current-file :which-key "sudo-edit-current-file")
   "ff" '(find-file :which-key "find-file")
   "fj" '(recentf :which-key "recentf")
   "fp" '(xah-copy-file-path :which-key "copy-file-path")
   "fr" '(consult-recent-file :which-key "open-recentf-file")
   "f/" '(rg-dwim-current-file :which-key "rg-dwim-current-file")
   ;; Git
   "g" '(nil :which-key "git")
   "gb" '(magit-blame :which-key "magit-blame")
   "gc" '(magit-blame-cycle-style :which-key "magit-blame-cycle-style")
   "gf" '(magit-file-checkout :which-key "magit-file-checkout")
   "gl" '(nil :which-key "log")
   "gld" '(magit-dired-log :which-key "magit-dired-log")
   "glf" '(magit-log-buffer-file :which-key "magit-log-buffer-file")
   "gm" '(vc-msg-show :which-key "vc-msg-show")
   "gM" '(transient-smerge :which-key "smerge-menu")
   "gs" '(magit :which-key "magit-status")
   "gt" '(magit-todos-list :which-key "magit-todos-list")
   "gT" '(git-timemachine :which-key "git-timemachine")
   ;; Help
   "h" '(nil :which-key "help")
   "hc" '(company-diag :which-key "company-diag")
   "hd" '(nil :which-key "details")
   "hdm" '((lambda () (interactive) (describe-variable 'major-mode)) :which-key "major-mode")
   "hdn" '((lambda () (interactive) (describe-variable 'minor-mode-list)) :which-key "minor-mode-list")
   "he" '(nil :which-key "emacs")
   "hek" '(kill-emacs :which-key "kill-emacs")
   "hep" '(dump-emacs :which-key "portable-dump-emacs")
   "her" '(restart-emacs :which-key "restart-emacs")
   "heu" '(package-upgrade-all :which-key "package-upgrade-all")
   "hev" '(view-lossage :which-key "view-lossage")
   "hf" '(helpful-callable :which-key "helpful-callable")
   "hF" '(describe-face :which-key "describe-face")
   "hi" '(info :which-key "emacs-info")
   "hk" '(helpful-key :which-key "helpful-key")
   "hm" '(describe-keymap :which-key "describe-keymap")
   "hs" '(describe-syntax :which-key "describe-syntax")
   "hS" '(transient-emacs-cheatsheet :which-key "my-emacs-cheatsheet")
   "hv" '(helpful-variable :which-key "helpful-variable")
   "hp" '(helpful-at-point :which-key "helpful-at-point")
   "hP" '(nil :which-key "profiler")
   "hPs" '(profiler-start :which-key "profiler-start")
   "hPr" '(profiler-report :which-key "profiler-report")
   "hPS" '(profiler-stop :which-key "profiler-Stop")
   "hw" '(nil :which-key "which-key")
   "hwk" '(which-key-show-top-level :which-key "show-top-level")
   ;; Jump
   "j" '(nil :which-key "jump/goto")
   "jc" '(avy-goto-char :which-key "avy-goto-char")
   "jd" '(dired-jump :which-key "dired-jump")
   "jD" '(dired-jump-other-window :which-key "dired-jump-other-window")
   "je" '(avy-goto-word-0 :which-key "avy-goto-word-0")
   "jf" '(find-file-at-point :which-key "find-file-at-point")
   "jj" '(avy-goto-char-in-line :which-key "avy-goto-char-in-line")
   "jl" '(avy-goto-line :which-key "avy-goto-line")
   "jw" '(avy-goto-word-1 :which-key "avy-goto-word-1")
   ;; Major
   "m" '(nil :which-key "major")
   ;; Navigator
   "n" '(nil :which-key "navigator")
   "nm" '(consult-imenu :which-key "imenu")
   "ns" '(nil :which-key "symbol-overlay")
   "nsc" '(symbol-overlay-remove-all :which-key "symbol-overlay-remove-all")
   "nsh" '(symbol-overlay-map-help :which-key "symbol-overlay-help")
   "nsi" '(symbol-overlay-put :which-key "symbol-overlay-input")
   ;; Project
   "p" '(nil :which-key "project")
   "p/" '(rg-project :which-key "ripgrep-search-project")
   "pb" '(project-switch-to-buffer :which-key "switch-to-project-buffer")
   "pd" '(project-dired :which-key "project-root-dired")
   "pf" '(project-find-file :which-key "project-find-file")
   "pF" '(project-find-dir :which-key "project-find-directory")
   "pk" '(project-kill-buffers :which-key "project-kill-buffers")
   "pp" '(my/project-switch-project :which-key "switch-project")
   ;; "pt" '(+neotree/find-project-root :which-key "neotree-find-project-root")
   "pt" '(+dirvish/project-root-side :which-key "+dirvish/project-root-side")
   ;; Text
   "t" '(nil :which-key "text")
   "tB" '(comment-box :which-key "comment-box")
   "ti" '(nil :which-key "insert")
   "tn" '(+funcs/narrow-or-widen-dwim :which-key "narrow-to-region")
   "tr" '(nil :which-key "replace")
   "trb" '(evilmr-replace-in-buffer :which-key "evilmr-replace-in-buffer")
   "trf" '(evilmr-replace-in-defun :which-key "evilmr-replace-in-defun")
   "trl" '(evilmr-replace-lines :which-key "evilmr-replace-lines")
   "ts" '(transient-text-scale :which-key "scale")
   "tS" '(transient-string-inflection :which-key "string-inflection-cycle")
   ;; "tt"  '(tiny-expand :which-key "tiny-expand")
   "tw" '(+funcs/shrink-whitespaces :which-key "shrink-whitespace")
   ;; View/Workspace/Layout
   "v" '(nil :which-key "workspace")
   "va" '(persp-add-buffer :which-key "persp-add-buffer")
   "vb" '(persp-switch-to-buffer :which-key "persp-switch-to-buffer")
   "vk" '(persp-kill :which-key "persp-kill")
   "vv" '(persp-switch :which-key "persp-switch")
   "vV" '(+persp/add-new-with-visible-buffers :which-key "my-persp-switch-init-view")
   ;; Window
   "w" '(nil :which-key "window")
   "w/" '(split-window-right :which-key "split-right")
   "w-" '(split-window-below :which-key "split-bottom")
   "wb" '(balance-windows :which-key "balance-windows")
   "wd" '(delete-window :which-key "delete-window")
   "wm" '(+funcs/toggle-maximize-buffer :which-key "maximized")
   "ws" '(transient-window-scale :which-key "scale")
   "wt" '(transient-transpose-frame :which-key "transpose-frame")
   "ww" '(ace-swap-window :which-key "swap-window")
   ;; Toggle
   "T" '(nil :which-key "toggle")
   "Td" '(toggle-debug-on-error :which-key "debug-on-error")
   "Tf" '(font-lock-mode :which-key "syntax-highlighting")
   "Tl" '(toggle-truncate-lines :which-key "truncate-lines")
   "Tn" '(display-line-numbers-mode :which-key "display-line-numbers")
   "Tp" '(nil :which-key "proxy")
   "Tph" '(proxy-http-toggle :which-key "http(s)-proxy")
   "Tps" '(proxy-socks-toggle :which-key "socks-proxy")
   "Tw" '(whitespace-mode :which-key "whitespace-mode")
   "Tx" '((lambda () (interactive) (shell-command "xmodmap ~/.Xmodmap")) :which-key "xmodmap")
   "Tz" '(evil-toggle-fold :which-key "evil-toggle-fold")))

(use-package lv
  :hook (lv-window . visual-line-mode))

(use-package transient
  :ensure nil
  :commands (transient-text-scale
             transient-window-scale
             transient-transpose-frame
             transient-emacs-cheatsheet
             transient-string-inflection)
  :config
  (setq transient-show-popup t)
  (defvar +transient/non-dedicated-display '(display-buffer-in-side-window
                                             (side . bottom)
                                             (dedicated . nil)
                                             (inhibit-same-window . t)
                                             (window-parameters (no-other-window . t))))

  (transient-define-prefix transient-text-scale ()
    "text scale"
    ["text scale"
     ("k" "default-text-scale-increase" default-text-scale-increase :transient t)
     ("j" "default-text-scale-decrease" default-text-scale-decrease :transient t)
     ("0" "default-text-scale-reset" default-text-scale-reset :transient t)
     ("K" "text-scale-increase" text-scale-increase :transient t)
     ("J" "text-scale-decrease" text-scale-decrease :transient t)
     ("q" "quit" transient-quit-all)])

  (transient-define-prefix transient-window-scale ()
    "window scale"
    ["window scale"
     ("h" "shrink-window-horizontally" shrink-window-horizontally :transient t)
     ("l" "enlarger-window-horizontally" enlarge-window-horizontally :transient t)
     ("j" "shrink-window" shrink-window :transient t)
     ("k" "enlarge-window" enlarge-window :transient t)
     ("b" "balance" balance-windows)
     ("q" "quit" transient-quit-all)])

  (define-advice transient-window-scale (:around (orig-fn) advice)
    (let ((transient-display-buffer-action +transient/non-dedicated-display))
      (funcall orig-fn)))

  (transient-define-prefix transient-transpose-frame ()
    "transpose frame"
    ["transpose frame
    |A|B|
    |C|D|"
     ("t" "transpose-frame            B<->C" transpose-frame)
     ("-" "flip-frame                 AB<->CD" flip-frame)
     ("/" "flop-frame                 AC<->BD" flop-frame)
     ("x" "rotate-frame               A<->D B<->C" rotate-frame)
     ("o" "rotate-frame-clockwise     A->B->D->C->A" rotate-frame-clockwise)
     ("O" "rotate-frame-anticlockwise A->C->D->B->A" rotate-frame-anticlockwise)
     ("q" "quit" transient-quit-all)])

  (transient-define-prefix transient-emacs-cheatsheet ()
    "emacs cheatsheet"
    ["Emacs Cheatsheet"
     ("s" "\"C-x TAB\" indent-rigidly" indent-rigidly) ; shift line[s] left/right
     ("q" "quit" transient-quit-all)])

  (transient-define-prefix transient-string-inflection ()
    ["Cycle text objects through camelCase, kebab-case, snake case and UPPER CASE."
     ("s" "string-inflection-all-cycle" string-inflection-all-cycle :transient t)
     ("q" "quit" transient-quit-all)]))


(provide 'init-keybinding)

;;; init-keybinding.el ends here

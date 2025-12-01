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
   "ad" '(docker :which-key "docker")
   "af" '(elfeed :which-key "elfeed")
   "am" '(man :which-key "man-page")
   "ao" '(nil :which-key "org")
   "aoa" '(org-agenda :which-key "org-agenda")
   "aoc" '(org-capture :which-key "org-capture")
   "ap" '(list-processes :which-key "list-processes")
   "aP" '(package-isolate :which-key "package-isolate") ;; fast emacs debug environment
   "aR" '(re-builder :which-key "re-builder")
   "at" '(telega :which-key "telega")
   "ay" '(nil :which-key "translate")
   "ayy" '(gt-translate :which-key "translate-at-point")
   "ayv" '(gt-speak :which-key "voice-at-point")
   ;; Buffers
   "b" '(nil :which-key "buffer")
   "bb" '(+funcs/switch-to-buffer-dwim :which-key "switch-to-buffer-dwim")
   "bB" '(+funcs/switch-to-buffer :which-key "switch-to-buffer")
   "bd" '(kill-current-buffer :which-key "kill-current-buffer")
   "bi" '(imenu-list-smart-toggle :which-key "imenu")
   "bI" '(ibuffer :which-key "buffers list")
   "bm" '((lambda () (interactive) (switch-to-buffer (messages-buffer))) :which-key "switch-to-<*Messages*>")
   "bn" '((lambda () (interactive) (+funcs/switch-empty-buffer-or-create "untitled")) :which-key "switch-to-empty-buffer")
   "bN" '(+funcs/new-empty-buffer :which-key "create-empty-buffer")
   "br" '(rename-buffer :which-key "rename-buffer")
   "bs" '(scratch-buffer :which-key "switch-to-<*scratch*>")
   ;; Files
   "f" '(nil :which-key "file")
   "fd" '(dirvish-fd :which-key "dirvish-fd")
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
   "gs" '(magit :which-key "magit-status")
   ;; "gt" '(magit-todos-list :which-key "magit-todos-list")
   "gt" '(consult-magit-todos :which-key "consult-magit-todos")
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
   "hv" '(helpful-variable :which-key "helpful-variable")
   "hp" '(describe-package :which-key "describe-package")
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
   "jD" '(dirvish :which-key "dirvish")
   "je" '(avy-goto-word-0 :which-key "avy-goto-word-0")
   "jf" '(find-file-at-point :which-key "find-file-at-point")
   "jj" '(avy-goto-char-in-line :which-key "avy-goto-char-in-line")
   "jl" '(avy-goto-line :which-key "avy-goto-line")
   "jw" '(avy-goto-word-1 :which-key "avy-goto-word-1")
   ;; Major
   "m" '(nil :which-key "major")
   ;; Navigator
   "n" '(nil :which-key "navigator")
   "nl" '(imenu-list :which-key "imenu-list")
   "nm" '(consult-imenu :which-key "conult-imenu")
   "no" '(symbols-outline-show :which-key "symbols-outline-show")
   "ns" '(nil :which-key "symbol-overlay")
   "nsc" '(symbol-overlay-remove-all :which-key "symbol-overlay-remove-all")
   "nsh" '(symbol-overlay-map-help :which-key "symbol-overlay-help")
   "nsi" '(symbol-overlay-put :which-key "symbol-overlay-input")
   ;; Project
   "p" '(nil :which-key "project")
   "p/" '(rg-project :which-key "ripgrep-search-project")
   ;; "pb" '(project-switch-to-buffer :which-key "switch-to-project-buffer")
   "pb" '(+funcs/projct-switch-to-buffer :which-key "switch-to-project-buffer")
   "pd" '(project-dired :which-key "project-root-dired")
   "pf" '(project-find-file :which-key "project-find-file")
   "pF" '(project-find-dir :which-key "project-find-directory")
   "pk" '(project-kill-buffers :which-key "project-kill-buffers")
   "pp" '(my/project-switch-project :which-key "switch-project")
   "pt" '(+dirvish/project-root-side :which-key "+dirvish/project-root-side")
   ;; Text
   "t" '(nil :which-key "text")
   "tB" '(comment-box :which-key "comment-box")
   "tn" '(+funcs/narrow-or-widen-dwim :which-key "narrow-to-region")
   "ts" '(transient-text-scale :which-key "scale")
   "tS" '(transient-string-inflection :which-key "string-inflection-cycle")
   "tw" '(+funcs/shrink-whitespaces :which-key "shrink-whitespace")
   ;; View/Workspace/Layout/Session
   "v" '(nil :which-key "workspace")
   ;; "vc" '(tab-new :which-key "tab-new")
   "vc" '(+workspace/tab-new :which-key "tab-new")
   "vn" '(tab-next :which-key "tab-next")
   "vp" '(tab-previous :which-key "tab-previous")
   "vr" '(tab-rename :which-key "tab-rename")
   "vs" '(tab-switch :which-key "tab-switch")
   "vx" '(tab-close :which-key "tab-close")
   "vD" '(easysession-delete :which-key "easysession-delete")
   "vR" '(easysession-switch-to :which-key "easysession-switch-to")
   "vS" '(easysession-save-as :which-key "easysession-save-as")
   ;; Window
   "w" '(nil :which-key "window")
   "w/" '(split-window-right :which-key "split-right")
   "w-" '(split-window-below :which-key "split-bottom")
   "wb" '(balance-windows :which-key "balance-windows")
   "wd" '(delete-window :which-key "delete-window")
   "wl" '(transient-windows-layout :which-key "windows-layout")
   "wm" '(+funcs/toggle-maximize-buffer :which-key "maximized")
   "ws" '(transient-window-scale :which-key "scale")
   "ww" '(ace-swap-window :which-key "swap-window")
   ;; Toggle
   "T" '(nil :which-key "toggle")
   "Td" '(toggle-debug-on-error :which-key "debug-on-error")
   "Te" '(next-error-follow-minor-mode :which-key "next-error-follow-minor-mode") ;; `compilation-next-error', `compilation-previous-error'
   "Tl" '(toggle-truncate-lines :which-key "truncate-lines")
   "Tn" '(display-line-numbers-mode :which-key "display-line-numbers")
   "Tp" '(nil :which-key "proxy")
   "Tph" '(proxy-http-toggle :which-key "http(s)-proxy")
   "Tps" '(proxy-socks-toggle :which-key "socks-proxy")
   "Tv" '(visual-line-mode :which-key "visual-line-mode")
   "Tw" '(whitespace-mode :which-key "whitespace-mode")
   ;; "Tx" '((lambda () (interactive) (shell-command "xmodmap ~/.Xmodmap")) :which-key "xmodmap")
   ))

;; Examples: https://github.com/positron-solutions/transient-showcase
(use-package transient
  :ensure nil
  :custom
  (transient-levels-file (locate-user-emacs-file (convert-standard-filename "cache/transient/levels.el")))
  (transient-values-file (locate-user-emacs-file (convert-standard-filename "cache/transient/values.el")))
  (transient-history-file (locate-user-emacs-file (convert-standard-filename "cache/transient/history.el")))
  :config
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (setq transient-show-popup t
        transient-save-history nil
        transient-display-buffer-action '((display-buffer-below-selected))
        transient-mode-line-format nil)
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
    (if (and (featurep 'posframe) (posframe-workable-p))
        (funcall orig-fn)
      (let ((transient-display-buffer-action +transient/non-dedicated-display))
        (funcall orig-fn))))

  (transient-define-prefix transient-windows-layout ()
    "window commands.
Check: https://p.bauherren.ovh/blog/tech/new_window_cmds"
    ["Transpose"
     ("t" "transpose-window-layout" transpose-window-layout :transient t)]
    ["Rotate layout"
     ("o" "rotate-window-layout-clockwise" rotate-window-layout-clockwise :transient t)
     ;; TODO: enable it
     ;; ("O" "rotate-window-layout-anticlockwise" rotate-window-layout-anticlockwise :transient t)
     ]
    ["Flip"
     ("-" "flip-window-layout-vertically" flip-window-layout-vertically :transient t)
     ("/" "flip-window-layout-horizontally" flip-window-layout-horizontally :transient t)]
    ["Cycle (rotate windows)"
     ("c" "rotate-windows" rotate-windows :transient t)
     ("C" "rotate-windows-back" rotate-windows-back :transient t)])

  (transient-define-prefix transient-string-inflection ()
    ["Cycle text objects through camelCase, kebab-case, snake case and UPPER CASE."
     ("s" "string-inflection-all-cycle" string-inflection-all-cycle :transient t)
     ("q" "quit" transient-quit-all)]))

;; posframe
(use-package posframe
  :defer t
  :config
  ;; https://github.com/emacsorphanage/transient-posframe/wiki
  (with-eval-after-load 'transient
    (setq transient-mode-line-format nil)
    (setq transient-display-buffer-action
          (list
           (lambda (buffer _)
             (posframe-show
              buffer
              :poshandler #'posframe-poshandler-frame-center
              :min-width transient-minimal-frame-width
              :lines-truncate t
              :left-fringe 8
              :right-fringe 8
              :internal-border-color (transient--prefix-color)
              :internal-border-width 2
              :override-parameters '((title . "transient-posframe")
                                     ;; (undecorated . nil)
                                     ))
             (get-buffer-window transient--buffer t))))))


(provide 'init-keybinding)

;;; init-keybinding.el ends here

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

;; keybindings with which-key,evil well supported
(use-package general)

(with-eval-after-load '(and evil general)
  (define-key global-map "\M-o" nil) ; disable `facemenu-keymap' default keybindings first
  (general-define-key
   :states '(normal visual motion insert emacs)
   :keymaps '(override global)
   :prefix "SPC" ;`:prefix' will only apply to evil states not listed in `general-non-normal-states'
   :global-prefix "M-o"
   "" nil
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "last-buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   "'" '(separedit :which-key "edit-comment")
   "!" '(shell-command :which-key "shell-command")
   ":" '(eval-expression :which-key "eval-expression")
   ";" '(comment-dwim-2 :which-key "comment-line")
   "0" '(neotree-show :which-key "neotree")
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
   "acw" '(websocket-client-open :which-key "websocket-client")
   "ad" '(docker :which-key "docker")
   "af" '(elfeed :which-key "elfeed")
   "aj" '(nil :which-key "jupyter")
   "aje" '(nil :which-key "emacs-jupyter-notebook")
   "ajeo" '(ein:notebooklist-open :which-key "ein:notebooklist-open")
   "ajes" '(ein:jupyter-server-start :which-key "ein:jupyter-server-start")
   "ajeS" '(ein:jupyter-server-stop :which-key "ein:jupyter-server-stop")
   "ajj" '(nil :which-key "emacs-jupyter")
   "ajjr" '(jupyter-run-repl :which-key "jupter-run-repl")
   "ajjR" '((lambda nil
              (interactive)
              (org-babel-jupyter-aliases-from-kernelspecs t)
              (message "Refresh kernelpsecs: %s" jupyter--kernelspecs))
            :which-key "refresh-kernelspecs")
   "ak" '(keycast-mode :which-key "keycast")
   "am" '(man :which-key "man-page")
   "ao" '(nil :which-key "org")
   "aoa" '(org-agenda :which-key "org-agenda")
   "aoc" '(org-capture :which-key "org-capture")
   "ap" '(list-processes :which-key "list-processes")
   "ar" '(nil :which-key "org-roam")
   "arc" '(org-roam-capture :which-key "org-roam-capture")
   "arf" '(org-roam-find-file :which-key "org-roam-find-file")
   "ari" '(org-roam-insert :which-key "org-roam-insert")
   "arI" '(org-roam-jump-to-index :which-key "org-roam-jump-to-index")
   "art" '(org-roam :which-key "org-roam-buffer-toggle-display")
   "aR" '(re-builder :which-key "re-builder")
   "at" '(telega :which-key "telega")
   "ay" '(nil :which-key "dictionary")
   "ays" '(youdao-dictionary-search-from-input :which-key "search-from-input")
   "ayt" '(powerthesaurus-lookup-word-dwim :which-key "powerthesaurus-lookup-word-dwim")
   "ayy" '(youdao-dictionary-search-at-point-posframe :which-key "translate-at-point")
   "ayv" '(youdao-dictionary-play-voice-at-point :which-key "voice-at-point")
   "aw" '(nil :which-key "emacs-browser")
   "awe" '(eaf-open-browser-with-history :which-key "eaf-open-browser-with-history")
   "aww" '(eww :which-key "open eww")
   "awb" '(eww-list-bookmarks :which-key "list-bookmarks")
   ;; Buffers
   "b" '(nil :which-key "buffer")
   "bb" '(+funcs/switch-to-buffer-dwim :which-key "switch-to-buffer-dwim")
   "bB" '(switch-to-buffer :which-key "switch-to-buffer")
   "bd" '(kill-current-buffer :which-key "kill-current-buffer")
   "bi" '(imenu-list-smart-toggle :which-key "imenu")
   "bI" '(ibuffer :which-key "buffers list")
   "bm" '((lambda () (interactive) (switch-to-buffer (messages-buffer))) :which-key "*Messages*")
   "bn" '((lambda () (interactive) (+funcs/switch-empty-buffer-or-create "untitled")) :which-key "empty-buffer")
   "bN" '(+funcs/new-empty-buffer :which-key "empty-buffer")
   "bs" '((lambda () (interactive) (+funcs/switch-buffer-or-create "*scratch*")) :which-key "*scratch*")
   ;; Files
   "f" '(nil :which-key "file")
   "fd" '(+dired/find-program :which-key "find-files-in-dired")
   "fe" '(+funcs/sudo-edit-current-file :which-key "sudo-edit-current-file")
   "ff" '(counsel-find-file :which-key "find-files")
   "fF" '(find-file-in-current-directory :which-key "find-file-in-current-directory")
   "fj" '(fasd-find-file :which-key "fasd-find-file")
   "fp" '(xah-copy-file-path :which-key "copy-file-path")
   "fr" '(counsel-recentf :which-key "counsel-recentf")
   "ft" '(counsel-tramp :which-key "counsel-tramp")
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
   "gt" '(git-timemachine :which-key "git-timemachine")
   "gT" '(magit-todos-list :which-key "magit-todos-list")
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
   "heu" '(upgrade-packages-async :which-key "upgrade-emacs-packages-async")
   "heU" '(upgrade-packages :which-key "upgrade-emacs-packages-sync")
   "hf" '(helpful-callable :which-key "helpful-callable")
   "hF" '(describe-face :which-key "describe-face")
   "hk" '(helpful-key :which-key "helpful-key")
   "hs" '(describe-syntax :which-key "describe-syntax")
   "hS" '(hydra-emacs-cheatsheet/body :which-key "my-emacs-cheatsheet")
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
   "nd" '(helm-dash-at-point :which-key "helm-dash-at-point")
   "nm" '(counsel-imenu :which-key "imenu")
   "ns" '(nil :which-key "symbol-overlay")
   "nsc" '(symbol-overlay-remove-all :which-key "symbol-overlay-remove-all")
   "nsh" '(symbol-overlay-map-help :which-key "symbol-overlay-help")
   "nsi" '(symbol-overlay-put :which-key "symbol-overlay-input")
   ;; Project
   "p" '(nil :which-key "project")
   "p/" '(rg-project :which-key "ripgrep-search-project")
   "p'" '(+shell/projectile-shell-pop :which-key "project-root-shell-pop")
   "pb" '(+project/ivy-switch-buffer :which-key "switch-to-project-buffer")
   "pB" '(projectile-ibuffer :which-key "project-ibuffer")
   "pd" '(projectile-dired :which-key "project-root-dired")
   "pf" '(find-file-in-project :which-key "project-find-file")
   "pF" '(find-directory-in-project :which-key "project-find-directory")
   "pg" '(counsel-git :which-key "counsel-git")
   "pk" '(projectile-kill-buffers :which-key "project-kill-buffers")
   "pp" '(projectile-switch-project :which-key "switch-project")
   "pt" '(+neotree/find-project-root :which-key "neotree-find-project-root")
   ;; Text
   "t" '(nil :which-key "text")
   "tB" '(comment-box :which-key "comment-box")
   "ti" '(nil :which-key "insert")
   "tit" '(insert-translated-name-insert :which-key "chinese->engish")
   "tm" '(hydra-multi-cursors/body :which-key "mutli-cursors")
   "tn" '(+funcs/narrow-or-widen-dwim :which-key "narrow-to-region")
   "tr" '(nil :which-key "replace")
   "trb" '(evilmr-replace-in-buffer :which-key "evilmr-replace-in-buffer")
   "trf" '(evilmr-replace-in-defun :which-key "evilmr-replace-in-defun")
   "trl" '(evilmr-replace-lines :which-key "evilmr-replace-lines")
   "ts" '(hydra-text-scale/body :which-key "scale")
   "tS" '(hydra-string-inflection/body :which-key "string-inflection-cycle")
   ;; "tt"  '(tiny-expand :which-key "tiny-expand")
   "tw" '(+funcs/shrink-whitespaces :which-key "shrink-whitespace")
   "t=" '(er/expand-region :which-key "expand-region")
   ;; View
   "v" '(nil :which-key "view")
   "vp" '(ivy-push-view :which-key "ivy-push-view")
   "vo" '(ivy-pop-view :which-key "ivy-pop-view")
   "vu" '((lambda () (interactive) (ivy-push-view 1)) :which-key "ivy-update-view")
   "vv" '(ivy-switch-view :which-key "ivy-switch-view")
   ;; Window
   "w" '(nil :which-key "window")
   "w/" '(split-window-right :which-key "split-right")
   "w-" '(split-window-below :which-key "split-bottom")
   "wb" '(balance-windows :which-key "balance-windows")
   "wd" '(delete-window :which-key "delete-window")
   "wm" '(+funcs/toggle-maximize-buffer :which-key "maximized")
   "ws" '(ace-swap-window :which-key "swap-window")
   "wS" '(hydra-window-scale/body :which-key "scale")
   "ww" '(writeroom-mode :which-key "writeroom-mode")
   "wz" '(zoom-mode :which-key "toggle-zoom-mode")
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

(use-package hydra)
(use-package lv
  :hook (lv-window . visual-line-mode))

(defhydra hydra-text-scale (:hint nil)
  "zoom"
  ("k" default-text-scale-increase "default-text-scale-increase")
  ("j" default-text-scale-decrease "default-text-scale-decrease")
  ("0" default-text-scale-reset "default-text-scale-reset")
  ("K" text-scale-increase "text-scale-increase")
  ("J" text-scale-decrease "text-scale-decrease")
  ("q" nil "quit"))

(defhydra hydra-window-scale (:hint nil)
  "scale window"
  ("h" shrink-window-horizontally "shrink-window-horizontally")
  ("l" enlarge-window-horizontally "enlarger-window-horizontally")
  ("j" shrink-window "shrink-window")
  ("k" enlarge-window "enlarge-window")
  ("b" balance-windows "balance")
  ("q" nil "quit"))

(defhydra hydra-emacs-cheatsheet (:hint nil :exit t)
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
  ("s" string-inflection-all-cycle "string-inflection-all-cycle")
  ("q" nil "quit"))


(provide 'init-keybinding)

;;; init-keybinding.el ends here

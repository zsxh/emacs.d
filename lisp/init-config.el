;; init-config.el --- Emacs config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs config
;;

;;; Code:

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))
(defconst IS-WSL     (if (and IS-LINUX (getenv "WSL_DISTRO_NAME")) t nil))

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved. We'll need everything we can get.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; slightly faster IO
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Coding System
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq-default kill-whole-line t)           ; Kill line including '\n'
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq auto-save-default nil)               ; Disable built in default auto save
(setq read-file-name-completion-ignore-case t) ; file ignores case
(setq-default truncate-lines t)
(setq use-dialog-box nil)                  ; no gui dialog box popups
(setq confirm-kill-processes nil)          ; just kill the process on exit
(setq column-number-mode t)                ; enable column number
;; Line numbers do not appear for very large buffers and buffers
;; with very long lines; see variables line-number-display-limit
;; check `line-number-mode'
(setq line-number-display-limit-width 1000)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(setq scroll-step 1)
(setq compilation-always-kill t
      compilation-scroll-output t)

(setq warning-minimum-level :error)

;; emacs 28 new feature, CJK word breaking lines
(setq word-wrap-by-category t)

;; disable the bell
(setq ring-bell-function 'ignore)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
;; M-^ delete-indentation
(setq-default c-basic-offset   2
              tab-width        2
              indent-tabs-mode nil)

;; (with-eval-after-load 'cc-vars
;;   (add-to-list 'c-offsets-alist '(case-label . +)))

;; normal cache location
(setq url-configuration-directory (locate-user-emacs-file "cache/url/")
      bookmark-default-file (locate-user-emacs-file "cache/bookmarks")
      nsm-settings-file (locate-user-emacs-file "cache/network-security.data")
      eww-bookmarks-directory (locate-user-emacs-file "cache/")
      recentf-save-file (locate-user-emacs-file "cache/recentf")
      tramp-persistency-file-name (locate-user-emacs-file "cache/tramp")
      project-list-file (locate-user-emacs-file "cache/projects")
      elfeed-db-directory (locate-user-emacs-file "cache/elfeed")
      transient-levels-file (locate-user-emacs-file "cache/transient/levels.el")
      transient-values-file (locate-user-emacs-file "cache/transient/values.el")
      transient-history-file (locate-user-emacs-file "cache/transient/history.el")
      package-quickstart-file (locate-user-emacs-file "cache/package-quickstart.el")
      rime-user-data-dir (locate-user-emacs-file "cache/rime/")
      tabspaces-session-file (locate-user-emacs-file "cache/tabsession.el"))

(setq mouse-drag-and-drop-region-cross-program t
      mouse-drag-and-drop-region t)

;; Increase subprocess read chunk size
(setq read-process-output-max (* 1024 1024))

;; repeat mode
(setq repeat-mode t)

;; improve long line coding, i don't need bidirectional text
;; https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)


(provide 'init-config)

;;; init-config.el ends here

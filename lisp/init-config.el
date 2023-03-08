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

;; Emacs startup *scratch* buffer
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; If non-nil and there was input pending at the beginning of the command,
;; the `fontification_functions` hook is not run.  This usually does not
;; affect the display because redisplay is completely skipped anyway if input
;; was pending, but it can make scrolling smoother by avoiding
;; unnecessary fontification.
;; It is similar to `fast-but-imprecise-scrolling' with similar tradeoffs,
;; but with the advantage that it should only affect the behavior when Emacs
;; has trouble keeping up with the incoming input rate.
(setq redisplay-skip-fontification-on-input t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a notable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

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

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash IS-MAC)

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
(setq make-backup-files nil)               ; Forbide to make backup files
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq-default kill-whole-line t)           ; Kill line including '\n'
(fset 'yes-or-no-p 'y-or-n-p)
;; (setq auto-save-default nil)               ; Disable built in default auto save
(setq read-file-name-completion-ignore-case t) ; file ignores case
(setq-default truncate-lines t)
(setq use-dialog-box nil)                  ; no gui dialog box popups
(setq confirm-kill-processes nil)          ; just kill the process on exit
(setq column-number-mode t)                ; enable column number
(setq x-wait-for-event-timeout 0)          ; no wait for X events
;; Line numbers do not appear for very large buffers and buffers
;; with very long lines; see variables line-number-display-limit
;; check `line-number-mode'
(setq line-number-display-limit-width 1000)
(setq inhibit-compacting-font-caches t)    ; don't compact font caches during GC

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(setq scroll-step 1)
(setq compilation-always-kill t
      compilation-scroll-output t)
(setq warning-minimum-level :error)
;; emacs 28 new feature, CJK word breaking lines
(when (boundp 'word-wrap-by-category)
  (setq word-wrap-by-category t))

;; Tab and Space
;; Permanently indent with spaces, never with TABs
;; M-^ delete-indentation
(setq-default c-basic-offset   2
              tab-width        2
              indent-tabs-mode nil)

(with-eval-after-load 'cc-vars
  (add-to-list 'c-offsets-alist '(case-label . +)))

;; normal cache location
(setq url-configuration-directory (locate-user-emacs-file "cache/url/")
      bookmark-default-file (locate-user-emacs-file "cache/bookmarks")
      nsm-settings-file (locate-user-emacs-file "cache/network-security.data")
      eww-bookmarks-directory (locate-user-emacs-file "cache/")
      recentf-save-file (locate-user-emacs-file "cache/recentf")
      tramp-persistency-file-name (locate-user-emacs-file "cache/tramp")
      project-list-file (locate-user-emacs-file "cache/projects")
      elfeed-db-directory (locate-user-emacs-file "cache/elfeed"))

(setq mouse-drag-and-drop-region-cross-program t
      mouse-drag-and-drop-region t)

;; Increase subprocess read chunk size
(when (bound-and-true-p read-process-output-max)
  (setq read-process-output-max (* 1024 1024)))

;; remap major mode
(when (treesit-available-p)
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (java-mode       . java-ts-mode)
          (js-mode         . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          ;; (ruby-mode       . ruby-ts-mode)
          (go-mode         . go-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (sh-mode         . bash-ts-mode)
          (rust-mode       . rust-ts-mode))))


(provide 'init-config)

;;; init-config.el ends here

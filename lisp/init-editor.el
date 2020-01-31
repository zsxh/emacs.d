;; init-editor.el --- Editor Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Editor Configurations
;;

;;; Code:

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
;; (setq use-dialog-box nil)                  ; no gui dialog box popups
(setq confirm-kill-processes nil)          ; just kill the process on exit
(setq column-number-mode t)                ; enable column number

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
;; auth-sources
(setq epg-pinentry-mode 'loopback)
;; (setq epa-file-select-keys 0)
;; ask encryption password once
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; (epa-file-enable)
(add-hook 'kill-emacs-hook (lambda () (shell-command "pkill gpg-agent")))

(setq scroll-step 1)
(setq compilation-always-kill t
      compilation-scroll-output t)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
;; M-^ delete-indentation
(setq-default c-basic-offset   2
              tab-width        2
              indent-tabs-mode nil)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :hook ((prog-mode
          org-mode
          dired-mode
          neotree-mode
          magit-mode
          conf-space-mode) . turn-on-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        ;; turn off `auto-revert-use-notify' or customize `auto-revert-notify-exclude-dir-regexp'
        ;; to exclude I/O intensive directories from auto-reverting.
        auto-revert-use-notify nil))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  :config
  ;; the reason to choose "region" not "lines" or "lines_or_region" is that
  ;; when i select the whole line(s), the end cursor is always at the beginning of
  ;; the line below the region, which make `line-end-position' return wrong value,
  ;; and cause wrong comment lines.
  (setq cd2/region-command 'cd2/comment-or-uncomment-region))

;; Edit comment or docstring in edit buffer
;; https://github.com/twlz0ne/comment-edit.el
(use-package comment-edit
  :quelpa ((comment-edit :fetcher github :repo "twlz0ne/comment-edit.el"))
  :commands (comment-edit)
  :config
  (with-eval-after-load '(and edit-indirect evil)
    (evil-define-minor-mode-key 'normal 'edit-indirect--overlay
      ",c" 'edit-indirect-commit
      ",k" 'edit-indirect-abort)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :commands ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Increase selected region by semantic units
(use-package expand-region
  :commands er/expand-region)

(setq mouse-drag-copy-region t)

;; Framework for mode-specific buffer indexes
(use-package imenu-list
  :commands imenu-list-smart-toggle
  :init
  (progn
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)))

;; Easy way to jump/swap window
(use-package ace-window
  :bind (("M-u" . ace-window))
  :commands (ace-window ace-swap-window)
  :config
  (with-eval-after-load 'neotree
    (add-to-list 'aw-ignored-buffers neo-buffer-name)))

;; Treat undo history as a tree
;; (use-package undo-tree
;;   :hook (after-init . global-undo-tree-mode))

(use-package undo-fu
  :config
  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-?") 'undo-fu-only-redo)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
    (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)))

;; Numbered window shortcuts
(use-package winum
  :hook (after-init . winum-mode))

;; Use package auto-save instead of default auto save
(use-package auto-save
  :quelpa ((auto-save :fetcher github :repo "manateelazycat/auto-save"))
  :preface
  ;; note: it's risky to disable lockfiles
  ;; https://emacs-china.org/t/filename/163/17
  (setq auto-save-default nil
        create-lockfiles nil)
  :config
  (setq auto-save-silent t
        auto-save-delete-trailing-whitespace t)
  (auto-save-enable))

;; jumping to visible text using a char-based decision tree
(use-package avy
  :bind (("C-c c" . avy-goto-char)
         ("C-c j" . avy-goto-char-in-line)
         ("C-c l" . avy-goto-line)
         ("C-c w" . avy-goto-word-1)
         ("C-c e" . avy-goto-word-0))
  ;; :config (setq avy-style 'pre)
  )

;; inserting numeric ranges
;; https://oremacs.com/2014/12/26/the-little-package-that-could/
(use-package tiny
  :commands tiny-expand
  :config (tiny-setup-default))

;; look through everything you've killed
(use-package browse-kill-ring
  :bind (("M-C-y" . browse-kill-ring)
         (:map browse-kill-ring-mode-map
               ("j" . browse-kill-ring-forward)
               ("k" . browse-kill-ring-previous)))
  :config
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item t))

;; rigrep
(use-package rg
  :bind ((:map rg-mode-map
               ("l" . nil)
               ("L" . rg-list-searches)))
  :commands (rg rg-dwim rg-project rg-literal rg-dwim-current-file))

;; wgrep allows you to edit a grep buffer and apply those changes to the file buffer
(use-package wgrep
  :defer t
  :config
  (advice-add 'wgrep-change-to-wgrep-mode :after (lambda () (evil-normal-state) (wgrep-toggle-readonly-area)))
  (advice-add 'wgrep-finish-edit :after (lambda () (evil-normal-state)))
  (advice-add 'wgrep-abort-changes :after (lambda () (evil-normal-state)))
  (evil-define-key 'normal wgrep-mode-map
    ",c" 'wgrep-finish-edit
    ",d" 'wgrep-mark-deletion
    ",r" 'wgrep-remove-change
    ",t" 'wgrep-toggle-readonly-area
    ",u" 'wgrep-remove-all-change
    ",k" 'wgrep-abort-changes
    "q" 'wgrep-exit))

;; https://github.com/rejeep/drag-stuff.el
;; Work fine with evil-mode
(use-package drag-stuff
  :after lispy
  :config
  (drag-stuff-global-mode 1)
  ;; bind "M-<up/down/left/right>" to move texts up/down/left/right
  (drag-stuff-define-keys))

;; Install fasd
;; https://github.com/clvv/fasd
(use-package fasd
  :commands (fasd-find-file))

;; This package allows Emacs to copy to and paste from the GUI clipboard
;; when running in text terminal.
(use-package xclip
  :if (not (display-graphic-p))
  :hook (after-init . xclip-mode))

;; https://github.com/ahungry/fast-scroll
;; It works by temporarily disabling font-lock and switching to a barebones mode-line,
;; until you stop scrolling (at which point it re-enables).
(use-package fast-scroll
  :hook (after-init . fast-scroll-mode)
  :config
  ;; If you would like to turn on/off other modes, like flycheck, add
  ;; your own hooks.
  ;; (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  ;; (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config))

(use-package darkroom
  :commands (darkroom-mode darkroom-tentative-mode))

;; keyboard macros
;;           Normal                         While defining macro
;;           ---------------------------    ------------------------------
;;  f3       Define macro                   Insert current counter value
;;           Prefix arg specifies initial   and increase counter by prefix
;;           counter value (default 0)      (default increment: 1)
;;
;;  C-u f3   APPENDs to last macro
;;
;;  f4       Call last macro                End macro
;;           Prefix arg specifies number
;;           of times to execute macro.
;;
;;  C-u f4   Swap last and head of macro ring.
;;
;;  S-mouse-3  Set point at click and       End macro and execute macro at
;;             execute last macro.          click.

(use-package so-long
  :ensure nil
  :if (fboundp 'global-so-long-mode)
  :hook (after-init . global-so-long-mode))

;; TODO: Better TRAMP autologin
;; https://www.eigenbahn.com/2020/01/15/tramp-autologin-insanity
(use-package tramp
  :ensure nil
  :defer t)


(provide 'init-editor)

;;; init-editor.el ends here

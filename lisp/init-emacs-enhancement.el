;; init-emacs-enhancement.el --- enhance emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  enhance emacs
;;

;;; Code:


;; just-in-time fontification
(use-package jit-lock
  :ensure nil
  :defer t
  :config
  ;; NOTE: [Re: Some performance questions.] https://lists.gnu.org/archive/html/emacs-devel/2023-02/msg00216.html
  ;; Turning on jit-stealth also lowers the GC pressure because it
  ;; fontifies buffers during idle time, so by the time you get to actually
  ;; editing a buffer it is already fontified, and thus all the garbage
  ;; produced by fontifications was already produced and collected; the
  ;; editing itself will produce much less garbage.
  (setq jit-lock-stealth-time 16
        jit-lock-stealth-load 100
        jit-lock-contextually t))

;; TreeSitter
(use-package treesit
  :if (featurep 'treesit)
  :ensure nil
  :defer t
  :config
  (setq-default treesit-font-lock-level 4)
  ;; (treesit-font-lock-recompute-features '(command string variable function operator bracket keyword))
  )

;;;;;;;;;;;;;; *Help* ;;;;;;;;;;;;;;

(use-package elisp-demos
  :defer t
  :init
  ;; Tips: bad performance with `company-box' doc request, so only enable elisp-demos for `helpful'
  ;; (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; A better *Help* buffer
(use-package helpful
  :defer t
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h f" . helpful-callable) ;; replace built-in `describe-function'
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal helpful-mode-map
      "gd" 'evil-goto-definition
      "gg" 'evil-goto-first-line
      "h" 'evil-backward-char
      "q" (lambda nil (interactive) (kill-buffer))))

  (when (featurep 'elisp-demos)
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;;;;;;;;;;;; *Buffer* ;;;;;;;;;;;;;;

(use-package ibuffer-vc
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer . (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

(use-package all-the-icons-ibuffer
  :defer t
  :init
  (add-hook-run-once 'ibuffer-hook (lambda () (all-the-icons-ibuffer-mode))))

;; TODO: try https://github.com/alphapapa/bufler.el

;;;;;;;;;;;;;; Setup a menu of recently opened files ;;;;;;;;;;;;;;
(use-package recentf
  :ensure nil
  :defer t
  :config
  (setq recentf-auto-cleanup "07:00pm"
        recentf-max-saved-items 200
        recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "undo-tree-hist"
                          "url"
                          "COMMIT_EDITMSG\\'"))
  (recentf-mode t))

;;;;;;;;;;;;;; Change priority of minor-mode keymaps ;;;;;;;;;;;;;;
(use-package minor-mode-hack
  :commands show-minor-mode-map-priority)

;;;;;;;;;;;;;; insert-char ;;;;;;;;;;;;;;
(use-package insert-char-preview
  :commands insert-char-preview
  :bind ("C-x 8 RET" . insert-char-preview))

;;;;;;;;;;;;;; posframe ;;;;;;;;;;;;;;
(use-package posframe
  :defer t
  :config
  (setq posframe-mouse-banish '(10000 . 10000)))

;;;;;;;;;;;;;; transient ;;;;;;;;;;;;;;
;; https://github.com/magit/transient/blob/master/docs/transient.org
(use-package transient
  :defer t
  :custom
  (transient-levels-file (locate-user-emacs-file (convert-standard-filename "cache/transient/levels.el")))
  (transient-values-file (locate-user-emacs-file (convert-standard-filename "cache/transient/values.el")))
  (transient-history-file (locate-user-emacs-file (convert-standard-filename "cache/transient/history.el")))
  :config
  (setq transient-display-buffer-action '((display-buffer-below-selected)))
  (define-key transient-map (kbd "<escape>") 'transient-quit-one))

;;;;;;;;;;;;;; Garbage-Collection ;;;;;;;;;;;;;;

(setq garbage-collection-messages t)

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
;;
;; https://gitlab.com/koral/gcmh
;; https://github.com/hlissner/doom-emacs/commit/717d53c6665229a731c55b23f9786c86111b3474
;; https://www.reddit.com/r/emacs/comments/bg85qm/garbage_collector_magic_hack/elniyfv?utm_source=share&utm_medium=web2x
;; https://github.com/hlissner/doom-emacs/issues/3108
;;
;; Follow the method recommended by Gnu Emacs Maintainer Eli Zaretskii: “My suggestion is
;; to repeatedly multiply gc-cons-threshold by 2 until you stop seeing significant improvements
;; in responsiveness, and in any case not to increase by a factor larger than 100 or somesuch.
;; If even a 100-fold increase doesn’t help, there’s some deeper problem with the Lisp code
;; which produces so much garbage, or maybe GC is not the reason for slowdown.”
;; https://www.reddit.com/r/emacs/comments/brc05y/is_lspmode_too_slow_to_use_for_anyone_else/eofulix/
(use-package gcmh
  :init
  :defer t
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-verbose t
        gcmh-low-cons-threshold 1800000))

;;;;;;;;;;;;;; Tramp ;;;;;;;;;;;;;;
;; TODO: Enhance Tramp
;; https://www.eigenbahn.com/2020/01/15/tramp-autologin-insanity
;; https://willschenk.com/articles/2020/tramp_tricks/
;; https://mina86.com/2021/emacs-remote/
(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh"
        remote-file-name-inhibit-cache 120
        tramp-verbose 3
        tramp-chunksize 2000))

(use-package tramp-cache
  :ensure nil
  :defer t)

;;;;;;;;;;;;;; Long Line Performance Improvement ;;;;;;;;;;;;;;
;; Emacs is now capable of editing files with very long lines since 29.1, `long-line-threshold'
(use-package so-long
  :ensure nil
  :if (version< emacs-version "29")
  :hook (after-init . global-so-long-mode))

(use-package profiler
  :ensure  nil
  :defer t
  ;; :config
  ;; https://www.murilopereira.com/how-to-open-a-file-in-emacs
  ;; (setf (caar profiler-report-cpu-line-format) 80
  ;;       (caar profiler-report-memory-line-format) 80)
  )


;;;;;;;;;;;;;; Calendar ;;;;;;;;;;;;;;
(use-package calendar
  :ensure nil
  :defer t
  :config
  ;; week starts on Monday
  (setq calendar-week-start-day 1)
  (add-hook-run-once 'calendar-mode-hook (lambda () (require 'cal-china-x))))

(use-package cal-china-x
  :defer t
  :config
  (require 'holidays)
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节"))
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays)))

;; Calfw - A calendar framework for Emacs
;; https://github.com/kiwanami/emacs-calfw
(use-package calfw-cal :defer t)
(use-package calfw-org :commands (cfw:open-org-calendar))
(use-package calfw
  :commands (cfw:open-calendar-buffer)
  :config
  (require 'calfw-org)

  (defun +calfw/auto-refresh-buffer (&rest _args)
    (cfw:refresh-calendar-buffer nil))

  (defun +calfw/setup ()
    (add-hook 'window-configuration-change-hook #'+calfw/auto-refresh-buffer nil 'local))

  (add-hook 'cfw:calendar-mode-hook #'+calfw/setup)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'cfw:calendar-mode 'normal)
    (evil-define-key 'normal cfw:calendar-mode-map
      "h" 'cfw:navi-previous-day-command
      "j" 'cfw:navi-next-week-command
      "k" 'cfw:navi-previous-week-command
      "l" 'cfw:navi-next-day-command
      "q" 'kill-current-buffer
      "<" 'cfw:navi-previous-month-command
      ">" 'cfw:navi-next-month-command
      (kbd "RET") 'cfw:org-open-agenda-day)
    (evil-define-key 'normal cfw:org-schedule-map
      "q" 'kill-current-buffer
      (kbd "RET") 'cfw:org-open-agenda-day)
    (evil-define-key 'normal cfw:org-custom-map
      "q" 'kill-current-buffer
      (kbd "RET") 'cfw:org-open-agenda-day)))

;; https://github.com/jacktasia/dumb-jump#obsolete-commands-and-options
(use-package dumb-jump
  :after xref
  :config
  (setq dump-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;;;;;;;;;;;; eldoc ;;;;;;;;;;;;;;

;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
(use-package eldoc
  :ensure nil
  :defer t
  :config
  ;; (global-eldoc-mode -1)
  (setq eldoc-echo-area-use-multiline-p 1
        eldoc-idle-delay 0.5)
  (eldoc-add-command-completions "delete-char" "lispy-delete-backward" "puni-backward-delete-char")
  (set-face-foreground 'eldoc-highlight-function-argument
                       (face-attribute 'font-lock-variable-name-face :foreground)))

;;;;;;;;;;;;;; others ;;;;;;;;;;;;;;

;; Toggle pixel scrolling, according to the turning of the mouse wheel
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(defun up-directory (path)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
	      (re-search-backward "/.")
	      (forward-char)
	      (delete-region (point) end))))

(define-key minibuffer-local-filename-completion-map
            [C-backspace] #'up-directory)

;; emacsql use emacs built-in sqlite
(use-package emacsql-sqlite-builtin
  :if (>= emacs-major-version 29)
  :defer t)

;;;;;;;;;;;;;; GnuPG and Auth Sources ;;;;;;;;;;;;;;
;; TODO: gpg and auth-sources
;; NOTE: https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(setq epg-pinentry-mode 'loopback)
;; (setq epa-file-select-keys 0)
;; ask encryption password once
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; (epa-file-enable)
;; (add-hook 'kill-emacs-hook (lambda () (shell-command "pkill gpg-agent")))



(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here

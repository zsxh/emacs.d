;; init-emacs-enhancement.el --- enhance emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  enhance emacs
;;

;;; Code:


;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :init
  (add-hook-run-once 'find-file-hook #'global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers nil
        auto-revert-verbose nil
        auto-revert-interval 5
        ;; turn off `auto-revert-use-notify' or customize `auto-revert-notify-exclude-dir-regexp'
        ;; to exclude I/O intensive directories from auto-reverting.
        auto-revert-use-notify t
        ;; Since checking a remote file is slow, these modes check or revert
        ;; remote files only if the user option `auto-revert-remote-files' is
        ;; non-nil.  It is recommended to disable version control for remote
        ;; files.
        auto-revert-remote-files nil
        ;; https://github.com/magit/magit/issues/2371#issuecomment-152746346
        ;; value nil, vc mode-line update when buffer changed. t, update every auto-revert-interval seconds
        auto-revert-check-vc-info t))

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
  ;; :preface
  ;; NOTE: `treesit-font-lock-level' has a special `setter' attached to it,
  ;; so as to automatically recompute the font lock features in all your buffers when you change the level
  ;; (custom-set-variables '(treesit-font-lock-level 4))
  )

;; NOTE: “Fixing” the S-Expression Commands, https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(defun mp-remove-treesit-sexp-changes ()
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function #'transpose-sexps-default-function))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function #'forward-sentence-default-function)))

(add-hook 'prog-mode-hook #'mp-remove-treesit-sexp-changes)

(use-package treesit-auto
  :if (featurep 'treesit)
  :defer t
  ;; :config
  ;; (global-treesit-auto-mode)
  )

;;;;;;;;;;;;;; *Help* ;;;;;;;;;;;;;;

(use-package elisp-demos
  :defer t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
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

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;;;;;;;;;;;; Setup a menu of recently opened files ;;;;;;;;;;;;;;
(use-package recentf
  :ensure nil
  :defer 10
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

(setq garbage-collection-messages nil)

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
  (setq gcmh-verbose nil
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
  (setq calendar-week-start-day 1))

;; `xref-find-definitions': <motion-state> gd
;; `xref-go-back': M-,
(use-package xref
  :ensure nil
  :defer nil
  :config
  (setq xref-history-storage 'xref-window-local-history))

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
  ;; `eldoc-echo-area-use-multiline-p'
  (setq eldoc-idle-delay 0.5)
  (eldoc-add-command-completions "delete-char" "lispy-delete-backward" "puni-backward-delete-char")
  (set-face-foreground 'eldoc-highlight-function-argument
                       (face-attribute 'font-lock-variable-name-face :foreground)))

;;;;;;;;;;;;;; GnuPG and Auth Sources ;;;;;;;;;;;;;;
;; TODO: gpg and auth-sources
;; NOTE: https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(setq epg-pinentry-mode 'loopback)
;; (setq epa-file-select-keys 0)
;; ask encryption password once
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; (epa-file-enable)
;; (add-hook 'kill-emacs-hook (lambda () (shell-command "pkill gpg-agent")))

;;;;;;;;;;;;;; Auto Save ;;;;;;;;;;;;;;
;; NOTE: For MacOS, https://emacs-china.org/t/macos-save-silently-t/24086
(setq save-silently t
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-visited-interval 1
      auto-save-visited-predicate
      (lambda () (and (not (string-match-p "\\.gpg\\'" buffer-file-name))
                      (not (string-equal "COMMIT_EDITMSG" (buffer-name)))))
      remote-file-name-inhibit-auto-save t
      remote-file-name-inhibit-auto-save-visited t)

(defun delete-trailing-whitespace-skip-current-line ()
    (interactive)
    (let ((begin (line-beginning-position))
          (end (point)))
      (when (< (point-min) begin)
        (delete-trailing-whitespace (point-min) (1- begin)))
      (when (> (point-max) end)
        (delete-trailing-whitespace end (point-max)))))

(add-to-list 'write-file-functions 'delete-trailing-whitespace-skip-current-line)

(add-hook 'after-init-hook #'auto-save-visited-mode)

;;;;;;;;;;;;;; Info-mode ;;;;;;;;;;;;;;
;; Extra colors for Emacs's `Info-mode'
(use-package info-colors
  :vc (:url "https://github.com/ubolonton/info-colors" :rev :newest)
  :hook (Info-selection . info-colors-fontify-node))

(with-eval-after-load 'info
  (set-face-foreground 'Info-quoted (face-foreground font-lock-constant-face)))

;;;;;;;;;;;;;; M-x breadcrumb-mode ;;;;;;;;;;;;;;
;; https://github.com/joaotavora/breadcrumb
(use-package breadcrumb
  :vc (:url "https://github.com/joaotavora/breadcrumb" :rev :newest)
  :defer t)

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


(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here

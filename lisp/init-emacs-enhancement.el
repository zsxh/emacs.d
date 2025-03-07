;; init-emacs-enhancement.el --- enhance emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  enhance emacs
;;

;;; Code:


;; Automatically reload files was modified by external program
(add-hook-run-once 'find-file-hook #'global-auto-revert-mode)
(with-eval-after-load 'autorevert
  (setq global-auto-revert-non-file-buffers nil
        auto-revert-verbose nil
        ;; Since checking a remote file is slow, these modes check or revert
        ;; remote files only if the user option `auto-revert-remote-files' is
        ;; non-nil.  It is recommended to disable version control for remote
        ;; files.
        auto-revert-remote-files nil
        ;; https://github.com/magit/magit/issues/2371#issuecomment-152746346
        ;; value nil, vc mode-line update when buffer changed. t, update every auto-revert-interval seconds
        auto-revert-check-vc-info t)
  (setopt auto-revert-interval 5
          ;; turn off `auto-revert-use-notify' or customize `auto-revert-notify-exclude-dir-regexp'
          ;; to exclude I/O intensive directories from auto-reverting.
          auto-revert-use-notify t))

;; just-in-time fontification
(with-eval-after-load 'jit-lock
  ;; NOTE: [Re: Some performance questions.] https://lists.gnu.org/archive/html/emacs-devel/2023-02/msg00216.html
  ;; Turning on jit-stealth also lowers the GC pressure because it
  ;; fontifies buffers during idle time, so by the time you get to actually
  ;; editing a buffer it is already fontified, and thus all the garbage
  ;; produced by fontifications was already produced and collected; the
  ;; editing itself will produce much less garbage.
  (setq jit-lock-stealth-time 1.5
        jit-lock-stealth-load 100
        jit-lock-stealth-nice 0.5
        jit-lock-contextually 'syntax-driven
        jit-lock-defer-time 0))

;; TreeSitter
;; (with-eval-after-load 'treesit
;;   ;; NOTE: `treesit-font-lock-level' has a special `setter' attached to it,
;;   ;; so as to automatically recompute the font lock features in all your buffers when you change the level
;;   (setopt treesit-font-lock-level 4))

;; NOTE: “Fixing” the S-Expression Commands, https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(defun mp-remove-treesit-sexp-changes ()
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function #'transpose-sexps-default-function))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function #'forward-sentence-default-function)))

(add-hook 'prog-mode-hook #'mp-remove-treesit-sexp-changes)
(add-hook 'html-ts-mode-hook #'mp-remove-treesit-sexp-changes)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; TODO: enable `markdown-ts-mode'
  (delete 'markdown treesit-auto-langs)
  ;; auto add to `auto-mode-alist' if `*-ts-mode' exists
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  ;; Make org src block respect `major-mode-remap-alist'
  (add-hook 'org-mode-hook #'treesit-auto--set-major-remap)
  (with-eval-after-load 'org
    (advice-add 'org-src-get-lang-mode :filter-return #'+funcs/try-get-major-mode-remap)))

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
                          "/ssh:"
                          "/sudo:"
                          "/tmp"
                          "/mnt"
                          "/var/folders"
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

;;;;;;;;;;;;;; Garbage-Collection ;;;;;;;;;;;;;;
(setq garbage-collection-messages nil)

;;;;;;;;;;;;;; Tramp ;;;;;;;;;;;;;;
;; https://www.eigenbahn.com/2020/01/15/tramp-autologin-insanity
;; https://willschenk.com/articles/2020/tramp_tricks/
;; https://mina86.com/2021/emacs-remote/
;; https://www.reddit.com/r/emacs/comments/1hccvoj/all_i_want_for_christmas_is_a_working_ssh_feature/
(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"
        tramp-verbose 3
        tramp-chunksize 2000))

;;;;;;;;;;;;;; Long Line Performance Improvement ;;;;;;;;;;;;;;
;; Emacs is now capable of editing files with very long lines since 29.1, `long-line-threshold'
(when (version< emacs-version "29")
  (add-hook 'after-init-hook #'global-so-long-mode))

;;;;;;;;;;;;;; Profiler ;;;;;;;;;;;;;;
;; https://www.murilopereira.com/how-to-open-a-file-in-emacs
;; (with-eval-after-load 'profiler
;;   (setf (caar profiler-report-cpu-line-format) 80
;;         (caar profiler-report-memory-line-format) 80))

;;;;;;;;;;;;;; Calendar ;;;;;;;;;;;;;;
;; Week starts on Monday
(with-eval-after-load 'calendar
  (setq calendar-week-start-day 1))

;;;;;;;;;;;;;; Xref ;;;;;;;;;;;;;;
;; `xref-find-definitions': <motion-state> gd
;; `xref-go-back': M-,
;; (use-package xref
;;   :ensure nil
;;   :defer t)

;; https://github.com/jacktasia/dumb-jump#obsolete-commands-and-options
(use-package dumb-jump
  :after xref
  :config
  (setq dump-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;;;;;;;;;;;;; eldoc ;;;;;;;;;;;;;;
;; Show function arglist or variable docstring
;; `global-eldoc-mode' is enabled by default.
;; `eldoc-echo-area-use-multiline-p'
(with-eval-after-load 'eldoc
  (setq eldoc-idle-delay 0.5)
  (eldoc-add-command-completions "delete-char" "lispy-delete-backward" "puni-backward-delete-char"))

;;;;;;;;;;;;;; GnuPG and Auth Sources ;;;;;;;;;;;;;;
;; NOTE: https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
(setq epg-pinentry-mode 'loopback)
;; (setq epa-file-select-keys 0)
;; ask encryption password once
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; (epa-file-enable)
;; (add-hook 'kill-emacs-hook (lambda () (shell-command "pkill gpg-agent")))

;; age encryption support for Emacs,
;; generate host key manually via `sudo ssh-keygen -A`
;; generate user key manually via `ssh-keygen -t ed25519 -a 256`
(use-package age
  :config
  (setq age-default-recipient '("~/.ssh/id_ed25519.pub")
        age-default-identity '("~/.ssh/id_ed25519")
        auth-sources (cons "~/.authinfo.age" auth-sources))

  (advice-add
   'age-file-find-file-hook
   :override
   (lambda ()
     "Check if 'secrets.nix' exists in the current buffer's directory.
If it exists, set the current buffer to read-only."
     (when-let* (buffer-file-name
                 (age-file-p (string-match age-file-name-regexp buffer-file-name))
                 (secrets-path-p (file-exists-p (expand-file-name "secrets.nix" default-directory))))
       (setq buffer-read-only t)
       (message
        "\"secrets.nix\" found, %s set to read-only. Use \"agenix\" to edit this file."
        (buffer-name))))))

;;;;;;;;;;;;;; Auto Save ;;;;;;;;;;;;;;
;; NOTE: For MacOS, https://emacs-china.org/t/macos-save-silently-t/24086
(setq save-silently t
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-visited-interval 1
      auto-save-visited-predicate
      (lambda ()
        (and (not (string-match-p "\\.gpg\\'" buffer-file-name))
             (not (string-match-p "\\.age\\'" buffer-file-name))
             (not (string-equal "COMMIT_EDITMSG" (buffer-name)))
             (not (bound-and-true-p rmsbolt-mode))))
      remote-file-name-inhibit-auto-save t
      remote-file-name-inhibit-auto-save-visited t
      remote-file-name-inhibit-cache 120)

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
  :vc (:url "https://github.com/ubolonton/info-colors")
  :hook (Info-selection . info-colors-fontify-node))

;;;;;;;;;;;;;; M-x breadcrumb-mode ;;;;;;;;;;;;;;
;; https://github.com/joaotavora/breadcrumb
(use-package breadcrumb
  :hook (after-init . breadcrumb-mode)
  :config
  (setq breadcrumb-project-max-length 0.3
        breadcrumb-imenu-max-length 0.5)

  (advice-add #'breadcrumb--format-project-node :around
              (lambda (og p more &rest r)
                "Icon For File"
                (let ((string (apply og p more r)))
                  (if more
                      string
                    (propertize
                     (concat (nerd-icons-icon-for-file string)
                             " " string)
                     'breadcrumb-dont-shorten t
                     'breadcrumb-with-icon t)))))

  (advice-add #'breadcrumb--format-ipath-node :around
              (lambda (og p more &rest r)
                "Icon for items"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (propertize
                       (concat (nerd-icons-codicon
                                "nf-cod-symbol_field"
                                :face 'breadcrumb-imenu-leaf-face)
                               " " string)
                       'breadcrumb-dont-shorten t
                       'breadcrumb-with-icon t)
                    (if (functionp 'nerd-icons-corfu--get-by-kind)
                        (propertize
                         (concat (nerd-icons-corfu--get-by-kind (intern (downcase string)))
                                 " " string)
                         'breadcrumb-with-icon t)
                      string)))))

  (advice-add #'breadcrumb--summarize :override
              (lambda (crumbs cutoff separator)
                (let ((rcrumbs
                       (cl-loop
                        for available = (- cutoff used)
                        for (c . more) on (reverse crumbs)
                        for seplen = (if more (length separator) 0)
                        for shorten-p = (unless (get-text-property 0 'breadcrumb-dont-shorten c)
                                          (> (+ (length c) seplen) available))
                        ;; NOTE: Include icon and first character
                        for toadd = (if shorten-p
                                        (if (get-text-property 0 'breadcrumb-with-icon c)
                                            (substring c 0 3)
                                          (substring c 0 1))
                                      c)
                        sum (+ (length toadd) seplen) into used
                        collect toadd)))
                  (string-join (reverse rcrumbs) separator)))))

;;;;;;;;;;;;;; Others ;;;;;;;;;;;;;;
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

(define-key minibuffer-local-filename-completion-map [C-backspace] #'up-directory)

;;;;;;;;;;;;;; Manual ;;;;;;;;;;;;;;
(with-eval-after-load 'man
  (setq Man-notify-method 'aggressive))

;;;;;;;;;;;;;; Ediff ;;;;;;;;;;;;;;
;; A comprehensive visual interface to diff & patch
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))


(provide 'init-emacs-enhancement)

;;; init-emacs-enhancement.el ends here

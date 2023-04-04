;; init-editor.el --- Editor Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Editor Configurations
;;

;;; Code:

;; Disable electric-indent-mode in org-mode
(use-package electric
  :ensure nil
  :hook (org-mode . (lambda () (electric-indent-local-mode -1))))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :hook ((prog-mode
          org-mode
          dired-mode
          neotree-mode
          magit-mode
          conf-space-mode) . +autorevert/turn-on)
  :init
  ;; disable auto revert in remote buffers
  (defun +autorevert/turn-on ()
    (unless (file-remote-p default-directory)
      (turn-on-auto-revert-mode)))
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        auto-revert-interval 10
        ;; turn off `auto-revert-use-notify' or customize `auto-revert-notify-exclude-dir-regexp'
        ;; to exclude I/O intensive directories from auto-reverting.
        auto-revert-use-notify nil
        ;; Since checking a remote file is slow, these modes check or revert
        ;; remote files only if the user option `auto-revert-remote-files' is
        ;; non-nil.  It is recommended to disable version control for remote
        ;; files.
        auto-revert-remote-files nil
        ;; https://github.com/magit/magit/issues/2371#issuecomment-152746346
        ;; value nil, vc mode-line update when buffer changed. t, update every auto-revert-interval seconds
        auto-revert-check-vc-info t)

  ;; NOTE: Reverts all buffers that have windows in the current frame to improve checking vc info performance
  ;; https://emacs.stackexchange.com/questions/28878/only-enable-auto-revert-mode-for-the-current-buffer
  (defvar auto-revert-some-buffers-filter #'get-buffer-window
    "Filter for the output of `buffer-list' in `auto-revert-buffers'.
The function is called with a buffer as argument.
It should return a non-nil value if this buffer should really be auto-reverted.")

  (defun auto-revert-some-buffers-advice--buffer-list (ret)
    "Filter output of the first call of `buffer-list' in `auto-revert-buffers'.
This filter de-installs itself after this call."
    (advice-remove #'buffer-list #'auto-revert-some-buffers-advice--buffer-list)
    (cl-remove-if-not auto-revert-some-buffers-filter ret))

  (defun auto-revert-some-buffers-advice (oldfun &rest args)
    "Filter the buffers to be auto-reverted through `auto-revert-some-buffers-filter' (which see)."
    (let (ret)
      (if global-auto-revert-mode
          (unwind-protect
              (progn
                (advice-add #'buffer-list :filter-return #'auto-revert-some-buffers-advice--buffer-list)
                (setq ret (apply oldfun args)))
            (advice-remove #'buffer-list #'auto-revert-some-buffers-advice--buffer-list) ;; being over-protective
            )
        (let ((old-auto-revert-buffer-list (cl-remove-if-not auto-revert-some-buffers-filter auto-revert-buffer-list))
              ;; Note: We interpret `auto-revert-remaining-buffers' as transient effect and don't filter this list.
              deleted-buffers)
          (let ((auto-revert-buffer-list old-auto-revert-buffer-list))
            (setq ret (apply oldfun args))
            (setq deleted-buffers (cl-set-difference old-auto-revert-buffer-list auto-revert-buffer-list)))
          (setq auto-revert-buffer-list (cl-set-difference auto-revert-buffer-list deleted-buffers))))
      ret))

  (advice-add #'auto-revert-buffers :around #'auto-revert-some-buffers-advice))

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
;; https://github.com/twlz0ne/separedit.el
(use-package separedit
  :commands (separedit)
  :config
  (with-eval-after-load 'tree-sitter-hl
    (add-to-list 'separedit-comment-faces 'tree-sitter-hl-face:comment))
  (with-eval-after-load 'evil
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

(use-package transpose-frame
  :commands (transpose-frame            ; \
             flip-frame                 ; -
             flop-frame                 ; |
             rotate-frame               ; x
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

;; Numbered window shortcuts
(use-package winum
  :defer 0.5
  :config
  (winum-mode))

;; jumping to visible text using a char-based decision tree
(use-package avy
  :bind (("C-c c" . avy-goto-char)
         ("C-c j" . avy-goto-char-in-line)
         ("C-c l" . avy-goto-line)
         ("C-c w" . avy-goto-word-1)
         ("C-c e" . avy-goto-word-0))
  ;; :config (setq avy-style 'pre)
  )

;; rigrep
(use-package rg
  :bind ((:map rg-mode-map
               ("l" . nil)
               ("L" . rg-list-searches)
               ("g" . nil)
               ("gr" . rg-recompile)))
  :commands (rg rg-dwim rg-project rg-literal rg-dwim-current-file)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal rg-mode-map
      "gg" 'evil-goto-first-line)))

;; wgrep allows you to edit a grep buffer and apply those changes to the file buffer
(use-package wgrep
  :defer t
  :config
  (with-eval-after-load 'evil
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
      "q" 'wgrep-exit)))

;; Install fasd
;; https://github.com/clvv/fasd
(use-package fasd
  :defer 15
  :commands (fasd-find-file)
  :config
  (global-fasd-mode))

;; This package allows Emacs to copy to and paste from the GUI clipboard
;; when running in text terminal.
(use-package xclip
  :if (not (display-graphic-p))
  :hook (after-init . xclip-mode))

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

;;;;;;;;;;;;;; EDIT ;;;;;;;;;;;;;;

;; automatic parenthesis pairing for non prog mode
(use-package elec-pair
  :ensure nil
  :hook ((prog-mode
          conf-mode
          yaml-mode
          editorconfig-mode
          vue-mode
          cider-repl-mode
          minibuffer-setup
          protobuf-mode
          ) . electric-pair-local-mode)
  :bind ("C-j" . newline-and-indent))

;; https://github.com/AmaiKinono/puni
;; Puni contains commands for soft deletion, which means deleting while keeping parentheses (or other delimiters, like html tags) balanced.
(use-package puni
  :defer t
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :config
  (define-key puni-mode-map (kbd "<") #'+editor/insert-angle)
  (define-key puni-mode-map (kbd "|") #'+editor/insert-rust-closure)

  (defun +editor/insert-angle ()
    "Insert angle brackets like intellij idea."
    (interactive)
    (save-excursion
      (let ((pos (point))
            (bounds (bounds-of-thing-at-point 'symbol)))
        (if bounds
            (let ((letter (char-after (car bounds))))
              (if (and (eq (upcase letter) letter)
                       (not (eq (downcase letter) letter)))
                  (insert "<>")
                (insert "<")))
          (insert "<"))))
    (forward-char))

  (defun +editor/insert-rust-closure ()
    (interactive)
    (save-excursion
      (if (and (member major-mode '(rust-mode rust-ts-mode))
               (eq ?\( (char-before)))
          (insert "||")
        (insert "|")))
    (forward-char))

  (with-eval-after-load 'rust-mode
    ;; Reset angle brackets syntax
    (modify-syntax-entry ?< "." rust-mode-syntax-table)
    (modify-syntax-entry ?> "." rust-mode-syntax-table))

  ;; HACK: String[]| `puni-backward-delete-char', check `forward-sexp', `forward-sexp-function', `treesit-forward-sexp' for details
  (define-advice puni-backward-delete-char (:around (orig-fn n) advice)
    (if (and
         (member major-mode '(java-ts-mode))
         (eq ?\] (char-before))
         (eq ?\[ (char-after
                  (scan-sexps (point) -1)))
         (eq (prefix-numeric-value n) 1))
        (backward-char)
      (funcall orig-fn n))))

;; Change variable name style
(use-package string-inflection
  :commands string-inflection-all-cycle)

;;;;;;;;;;;;;; Quick Scroll line ;;;;;;;;;;;;;;
;; keymap ("C-l" 'recenter-top-bottom) cycling 25%,top,bottom line position
(add-hook 'prog-mode-hook (lambda () (setq-local recenter-positions '(0.25 top bottom))))

;;;;;;;;;;;;;; Code Folding ;;;;;;;;;;;;;;
;; evil open/close/toggle folds rely on hideshow
;; "z a" evil-toggle-fold
;; "z m" evil-close-folds
;; "z r" evil-open-folds
(use-package hideshow
  :commands (hs-minor-mode hs-hide-level)
  :init
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "zf") 'hs-hide-level))
  :hook ((prog-mode nxml-mode) . hs-minor-mode)
  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

;; NOTE: RMSBolt tries to make it easy to see what your compiler is doing.
;; It does this by showing you the assembly output of a given source code file.
;; https://gitlab.com/jgkamat/rmsbolt
(use-package rmsbolt
  :commands rmsbolt-mode)

;;;;;;;;;;;;;; Coding styles for multiple developers working on the same project across various editors and IDEs ;;;;;;;;;;;;;;

(use-package editorconfig
  :hook (emacs-startup . editorconfig-mode)
  :config
  ;; use `auto-save' package to deal with trailing whitespace
  (setq editorconfig-trim-whitespaces-mode (lambda (arg) nil)))

;; TODO: formatter for python
;; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :defer t)


(provide 'init-editor)

;;; init-editor.el ends here

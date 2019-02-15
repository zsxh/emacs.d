;; init-debugger.el --- Debugger Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Debugger Settings
;;

;;; Code:

;; Debug/edebug evil keybindings
(with-eval-after-load 'evil-collection
  (with-eval-after-load 'debug
    (evil-collection-init 'debug)
    (evil-define-key 'normal debugger-mode-map (kbd "<return>") 'backtrace-help-follow-symbol))
  (with-eval-after-load 'edebug
    (evil-collection-init 'edebug)))

;; Realgud Configs
(use-package realgud
  :ensure t
  :commands (realgud:gdb realgud:ipdb realgud:jdb)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'realgud)))

;;;;;;;;;;;;;; Debug Adapter Protocol for Emacs ;;;;;;;;;;;;;;

(use-package dap-mode
  :quelpa ((dap-mode :fetcher github :repo "yyoncho/dap-mode"))
  :commands dap-mode
  :init
  (defun +dap/enable ()
    (dap-mode 1)
    (dap-ui-mode 1))
  (advice-add 'lsp :after #'+dap/enable)
  :config
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0))))

;; add minor mode(keybindings) for lsp debug
(with-eval-after-load 'dap-mode
  (defvar +dap/debug-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") 'dap-next)
      (define-key map (kbd "s") 'dap-step-in)
      (define-key map (kbd "o") 'dap-step-out)
      (define-key map (kbd "b") 'dap-breakpoint-toggle)
      (define-key map (kbd "B") 'dap-breakpoint-condition)
      (define-key map (kbd "c") 'dap-continue)
      (define-key map (kbd "C") 'dap-disconnect)
      map)
    "my dap mode debug keybindings")

  (define-minor-mode +dap/debug-mode
    "A minor mode for dap debug key settings."
    :init-value nil
    :keymap +dap/debug-mode-map)

  (define-global-minor-mode +dap/global-debug-mode +dap/debug-mode
    (lambda ()
      (when (memq major-mode '(java-mode python-mode))
        (+dap/debug-mode))))

  (with-eval-after-load 'evil
    (defun +dap/evil-debug-key-settings ()
      (evil-define-key 'normal +dap/debug-mode-map
        "n" 'dap-next
        "s" 'dap-step-in
        "o" 'dap-step-out
        "b" 'dap-breakpoint-toggle
        "B" 'dap-breakpoint-condition
        "c" 'dap-continue
        "C" 'dap-disconnect))
    (add-hook '+dap/debug-mode-hook #'+dap/evil-debug-key-settings))

  (defun +dap/debug-mode-disable (&optional args)
    (when +dap/global-debug-mode
      (+dap/global-debug-mode -1)
      (let ((inhibit-message t))
        (message "+dap/global-debug-mode disabled"))))

  (defun +dap/debug-mode-enable (&optional args)
    (unless +dap/global-debug-mode
      (+dap/global-debug-mode)
      ;; `evil-define-key' for minor mode does not take effect until a state transition
      ;; Issue: https://github.com/emacs-evil/evil/issues/301
      (when (bound-and-true-p evil-mode)
        (if (eq evil-state 'normal)
            (progn
              (evil-change-state 'emacs)
              (evil-change-state 'normal))
          (progn
            (let ((cur-state evil-state))
              (evil-change-state 'normal)
              (evil-change-state cur-state)))))
      (let ((inhibit-message t))
        (message "+dap/global-debug-mode enabled"))))

  ;; Manual toggle
  (defun +dap/debug-key-settings--toggle ()
    (interactive)
    (if +dap/debug-mode
        (+dap/debug-mode-disable)
      (+dap/debug-mode-enable)))

  ;; Auto toggle
  (add-hook 'dap-session-created-hook #'+dap/debug-mode-enable)
  (add-hook 'dap-terminated-hook #'+dap/debug-mode-disable))


(provide 'init-debugger)

;;; init-debugger.el ends here

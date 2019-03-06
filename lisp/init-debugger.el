;; init-debugger.el --- Debugger Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Debugger Settings
;;

;;; Code:

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

  ;; you neead to install several debugger first: lldb, ptvsd, eclipse jdt server, etc. links below:
  (require 'dap-gdb-lldb) ;https://github.com/emacs-lsp/dap-mode#native-debug-gdblldb
  (require 'dap-python)  ;https://github.com/emacs-lsp/dap-mode#python
  (require 'dap-java)    ;https://github.com/emacs-lsp/dap-mode#java

  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1)))

  (defun +dap/after-starting-debugging (debug-args)
    (if (hash-table-empty-p (dap--get-breakpoints))
        (switch-to-buffer-other-window (dap--debug-session-output-buffer (dap--cur-session-or-die)))
      (hydra-debugger-control/body)))

  (advice-add 'dap-start-debugging :after '+dap/after-starting-debugging))

(with-eval-after-load 'dap-mode

  ;; Debugger Cheatsheet
  (defhydra hydra-debugger-control (:color purple :hint nil :foreign-keys run)
    "
^Stepping^             ^Switch^           ^Breakpoints^          ^Eval
^^^^^^^^-----------------------------------------------------------------
_n_: Next          _ss_: Session          _bt_: Toggle          _ee_: Eval
_i_: Step in       _st_: Thread           _bd_: Delete          _er_: Eval region
_o_: Step out      _sf_: Stack frame      _ba_: Add             _es_: Eval thing at point
_c_: Continue      _sl_: List locals      _bc_: Set condition   _eii_: Inspect
_r_: Restart frame _sb_: List breakpoints _bh_: Set hit count   _eir_: Inspect region
_Q_: Disconnect    _sS_: List sessions    _bl_: Set log message _eis_: Inspect thing at point
^^                 ^^                     _bD_: Delete all
"
    ("n" dap-next)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("c" dap-continue)
    ("r" dap-restart-frame)
    ("Q" dap-disconnect)

    ("ss" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sS" dap-ui-sessions)

    ("bt" dap-breakpoint-toggle)
    ("bd" dap-breakpoint-delete)
    ("bD" dap-breakpoint-delete-all)
    ("ba" dap-breakpoint-add)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)

    ("ee" dap-eval)
    ("er" dap-eval-region)
    ("es" dap-eval-thing-at-point)
    ("eii" dap-ui-inspect)
    ("eir" dap-ui-inspect-region)
    ("eis" dap-ui-inspect-thing-at-point)

    ("q" nil "quit")))


;; Set up keybindings for dap-debugger
;; (with-eval-after-load 'dap-mode

;;   (defvar +dap/debug-mode-session-buffers (make-hash-table :test 'equal)
;;     "List of buffers that are associated with the session")

;;   (defvar +dap/debug-mode-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "n") 'dap-next)
;;       (define-key map (kbd "s") 'dap-step-in)
;;       (define-key map (kbd "o") 'dap-step-out)
;;       (define-key map (kbd "b") 'dap-breakpoint-toggle)
;;       (define-key map (kbd "B") 'dap-breakpoint-condition)
;;       (define-key map (kbd "c") 'dap-continue)
;;       (define-key map (kbd "C") 'dap-disconnect)
;;       map)
;;     "my dap mode debug keybindings")

;;   (define-minor-mode +dap/debug-mode
;;     "A minor mode for dap debug key settings."
;;     :init-value nil
;;     :keymap +dap/debug-mode-map)

;;   (define-global-minor-mode +dap/global-debug-mode +dap/debug-mode
;;     (lambda ()
;;       (when (memq major-mode '(java-mode python-mode c-mode c++-mode))
;;         (+dap/debug-mode))))

;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state '+dap/debug-mode 'normal)
;;     (evil-make-overriding-map +dap/debug-mode-map))

;;   (defun +dap/debug-mode-enable ()
;;     (unless +dap/debug-mode
;;       (+dap/debug-mode)
;;       ;; `evil-define-key' for minor mode does not take effect until a state transition
;;       ;; Issue: https://github.com/emacs-evil/evil/issues/301
;;       (when (bound-and-true-p evil-mode)
;;         (if (eq evil-state 'normal)
;;             (progn
;;               (evil-change-state 'emacs)
;;               (evil-change-state 'normal))
;;           (progn
;;             (let ((cur-state evil-state))
;;               (evil-change-state 'normal)
;;               (evil-change-state cur-state)))))))

;;   (defun +dap/debug-mode-disable ()
;;     (when +dap/debug-mode
;;       (+dap/debug-mode -1)))

;;   (defun +dap/enable-and-mark (&optional debug-session)
;;     (unless +dap/debug-mode
;;       (+dap/debug-mode-enable)
;;       (let* ((debug-session (if debug-session debug-session (dap--cur-session)))
;;              (session-name (dap--debug-session-name debug-session))
;;              (buffer-list (gethash session-name +dap/debug-mode-session-buffers)))
;;         (push (current-buffer) buffer-list)
;;         (puthash session-name buffer-list +dap/debug-mode-session-buffers))))

;;   (defun +dap/disable-and-unmark (&optional debug-session)
;;     (let* ((debug-session (if debug-session debug-session (dap--cur-session)))
;;            (session-name (dap--debug-session-name debug-session))
;;            (buffer-list (gethash session-name +dap/debug-mode-session-buffers))
;;            (inhibit-message t))
;;       (dolist (cur-buffer buffer-list)
;;         (with-current-buffer cur-buffer
;;           (+dap/debug-mode-disable)))
;;       (remhash session-name +dap/debug-mode-session-buffers)
;;       (message "+dap/debug-mode disabled :session %s" session-name)))

;;   ;; Manual toggle
;;   (defun +dap/debug-key-settings--toggle ()
;;     (interactive)
;;     (if +dap/debug-mode
;;         (+dap/debug-mode-disable)
;;       (+dap/debug-mode-enable)))

;;   ;; Auto toggle
;;   ;; somethings wrong with dap-session-create-hook's argument 'debug-session', mark buffer when dap--go-to-stack-frame instead
;;   ;; (add-hook 'dap-session-created-hook #'+dap/debug-mode-enable)
;;   (advice-add 'dap--go-to-stack-frame :after
;;               (lambda (debug-session stack-frame)
;;                 (+dap/enable-and-mark debug-session)))
;;   (add-hook 'dap-terminated-hook #'+dap/disable-and-unmark))


(provide 'init-debugger)

;;; init-debugger.el ends here

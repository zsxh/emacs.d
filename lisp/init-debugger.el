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
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :config
  (require 'dap-utils)
  ;; you neead to install several debugger first: lldb, ptvsd, eclipse jdt server, etc. links below:
  (require 'dap-gdb-lldb) ;https://github.com/emacs-lsp/dap-mode#native-debug-gdblldb
  (require 'dap-python)  ;https://github.com/emacs-lsp/dap-mode#python
  (when (featurep 'lsp-java)
    (require 'dap-java)))

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

    ("q" nil "quit"))

  ;; Display debug windows on session startup
  ;; https://github.com/emacs-lsp/dap-mode/wiki/HowTo:-Display-debug-windows-on-session-startup
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1)))

  (defun +dap/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun +dap/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        ;; display locals
        (unless (+dap/window-visible dap-ui--locals-buffer)
          (dap-ui-locals))
        ;; display sessions
        (unless (+dap/window-visible dap-ui--sessions-buffer)
          (dap-ui-sessions)))))

  (defun +dap/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--sessions-buffer)
           (kill-buffer dap-ui--sessions-buffer))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))))

  ;; (add-hook 'dap-stopped-hook (lambda (debug-session) (hydra-debugger-control/body)))
  (add-hook 'dap-stopped-hook '+dap/show-debug-windows)
  (add-hook 'dap-terminated-hook '+dap/hide-debug-windows)

  (defvar +dap-running-session-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") 'dap-next)
      (define-key map (kbd "i") 'dap-step-in)
      (define-key map (kbd "o") 'dap-step-out)
      (define-key map (kbd "c") 'dap-continue)
      (define-key map (kbd "r") 'dap-restart-frame)
      (define-key map (kbd "Q") 'dap-disconnect)
      (define-key map (kbd "b") 'dap-breakpoint-toggle)
      (define-key map (kbd "B") 'dap-breakpoint-condition)
      map)
    "Keybindings for +dap-runnong-session-mode")

  (defvar +dap-running-session-buffers (make-hash-table :test 'equal)
    "List of buffers that are associated with the session")

  ;; activate minor modes when stepping through code
  ;; https://github.com/emacs-lsp/dap-mode/wiki/How-to-activate-minor-modes-when-stepping-through-code
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil
    nil
    +dap-running-session-mode-map
    (with-eval-after-load 'evil
      (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
      (evil-make-overriding-map +dap-running-session-mode-map))
    ;; The following code adds to the dap-terminated-hook
    ;; so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-disable session)))))))

  (defun +dap-running-session-enable (debug-session)
    ;; (message (format "%s %s" (current-buffer) "enable +dap-...."))
    (+dap-running-session-mode 1)
    (let* ((session-name (dap--debug-session-name debug-session))
           (buffer-list (gethash session-name +dap-running-session-buffers)))
      (push (current-buffer) buffer-list)
      (puthash session-name buffer-list +dap-running-session-buffers)))

  (defun +dap-running-session-disable (debug-session)
    (let* ((session-name (dap--debug-session-name debug-session))
           (buffer-list (gethash session-name +dap-running-session-buffers)))
      (dolist (cur-buffer buffer-list)
        (with-current-buffer cur-buffer
          ;; (message (format "%s %s" (current-buffer) "disable enable +dap-...."))
          (+dap-running-session-mode -1)))
      (remhash session-name +dap-running-session-buffers)))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook '+dap-running-session-enable)

  ;; Deactivate after a debug session has been terminated.
  (add-hook 'dap-terminated-hook '+dap-running-session-disable)

  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-enable session)))))


(provide 'init-debugger)

;;; init-debugger.el ends here

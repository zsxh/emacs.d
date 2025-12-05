;;; zsxh-tmux.el --- Seamless tmux session and window management from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL:
;; Version: 0.1.0
;; Package-Requires: ((emacs "30"))

;; This file is not part of GNU Emacs.

;; Commentary:
;;
;; 2025/12/05: Deprecated, use `emamux' instead
;;
;; This package provides integration between Emacs, terminal and tmux.
;; It allows opening project directories in terminal with tmux sessions.
;;
;; Features:
;; - List existing tmux sessions and windows
;; - Create new tmux sessions with custom names
;; - Create new tmux windows with custom names and working directories
;; - Interactive selection of sessions/windows with completion
;; - Run commands in selected tmux sessions/windows
;; - Project-aware directory selection for new windows
;;
;; Usage:
;; M-x tmux-run
;;
;; Requirements:
;; - tmux

;;; Code:

(defcustom tmux-debug nil
  "Print tmux command if tmux-debug is not nil")

(defun tmux--string-empty-p (str)
  (if (or (null str)
          (string= str ""))
      t
    nil))

(defun tmux--list-sessions ()
  "Get list of available tmux sessions."
  (let ((sessions (shell-command-to-string "tmux list-sessions -F '#{session_name}'")))
    (unless (or (string-empty-p sessions)
                (string-prefix-p "no server running" sessions))
      (split-string sessions "\n" t))))

(defun tmux--list-windows (session)
  "Get list of available tmux windows in SESSION."
  (let ((windows (shell-command-to-string (format "tmux list-windows -t '%s' -F '#{window_id}:#{window_name}:#{pane_current_path}'" session))))
    (when (not (string-empty-p windows))
      (split-string windows "\n" t))))

(defun tmux--create-session (&optional name dir)
  "Create new tmux session with NAME,
setting working-dircotry to DIR."
  (let ((cmd "tmux new -d -P -F '#{session_name}'"))
    (unless (tmux--string-empty-p name)
      (setq cmd (concat cmd (format " -s '%s'" name))))
    (unless (tmux--string-empty-p dir)
      (setq cmd (concat cmd (format " -c %s" dir))))
    (when tmux-debug
      (message "%s" cmd))
    (string-trim (shell-command-to-string cmd))))

(defun tmux--create-window (&optional session dir name)
  "Create new tmux window with NAME in SESSION,
setting working-dircotry to DIR."
  (let ((cmd "tmux new-window -P -F '#{session_name}:#{window_id}'"))
    (unless (tmux--string-empty-p session)
      ;; -t <session_name>: 在最后加冒号保证 -t 后面加的是会话名而不是索引号
      (setq cmd (concat cmd (format " -t '%s:'" session))))
    (unless (tmux--string-empty-p dir)
      (setq cmd (concat cmd (format " -c %s" dir))))
    (unless (tmux--string-empty-p name)
      (setq cmd (concat cmd (format " -n '%s'" name))))
    (when tmux-debug
      (message "%s" cmd))
    (string-trim (shell-command-to-string cmd))))

(defun tmux--select-or-create-session (&optional session-name working-dir)
  "Let user select existing tmux session or create new one."
  (let* ((sessions (tmux--list-sessions))
         (choice (or session-name (completing-read
                                   "Select or Create tmux session"
                                   sessions))))
    (if (member choice sessions)
        choice
      (when (y-or-n-p (format "Create new session '%s'? " choice))
        (let ((working-dir (or working-dir
                               (read-directory-name
                                "New window working-directory: "
                                (expand-file-name
                                 (or (project-root (project-current))
                                     default-directory))))))
          (tmux--create-session choice working-dir))))))

(defun tmux--select-or-create-window (&optional session-name working-dir)
  "Let user select existing tmux window or create new one in SESSION.
Checks if current buffer is remote (e.g. tramp) and errors if true."
  (when (and (or (buffer-file-name) default-directory)
             (file-remote-p (or (buffer-file-name) default-directory)))
    (user-error "Remote files/directories not supported for tmux windows"))

  (let* ((session (tmux--select-or-create-session session-name))
         (windows (tmux--list-windows session))
         (choice
          (or
           (and working-dir (cl-find-if
                             (lambda (cand) (string-suffix-p working-dir cand))
                             windows))
           (completing-read "Select or Create tmux window: " windows))))
    (if (member choice windows)
        (format "%s:%s" session (substring choice 0 (string-search ":" choice)))
      (when (y-or-n-p (format "Create new window '%s'? " choice))
        (let ((working-dir (or working-dir
                               (read-directory-name
                                "New window working-directory: "
                                (expand-file-name
                                 (or (project-root (project-current))
                                     default-directory))))))
          (when (file-remote-p working-dir)
            (user-error "Cannot create tmux window with remote directory"))
          (tmux--create-window session working-dir choice))))))

;;;###autoload
(defun tmux-run (session-window cmd)
  "Run CMD in specified tmux SESSION-WINDOW.
Interactively prompts for:
1. A tmux session and window (existing or new)
2. A command to execute in that window

SESSION-WINDOW should be in format \"session_name:window_id\".
CMD is the shell command to execute in the target window.

Example:
  (tmux-run \"mysession:@1\" \"ls -la\")"
  (interactive
   (list (tmux--select-or-create-window)
         ;; TODO: https://github.com/LemonBreezes/emacs-fish-completion
         (read-string "tmux run: ")))

  (unless (executable-find "tmux")
    (user-error "tmux not found in PATH"))

  (call-process "tmux" nil nil nil
                "send-keys"
                "-t" session-window
                cmd
                "C-m")
  (message "Run \"%s\" in tmux %s" cmd session-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eat integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eat-tmux-open (&optional arg)
  (let* ((dir-remote-p (file-remote-p default-directory))
         (project (unless dir-remote-p (project-current)))
         (project-working-dir-p (and (not arg) project))
         (session-name (string-replace
                        "." "_"
                        (if project-working-dir-p
                            (project-name project)
                          "Default")))
         (working-dir (expand-file-name
                       (if project-working-dir-p
                           (project-root project)
                         default-directory)))
         (tmux-session (if project-working-dir-p
                           (tmux--select-or-create-session session-name working-dir)
                         (tmux--select-or-create-window session-name working-dir))))
    (unless eat-terminal
      (user-error "eat-termninal process not running"))
    (eat-term-send-string
     eat-terminal
     ;; FIXME: multiple tmux clients
     (format "tmux switchc -t %s || tmux attach -t %s" tmux-session tmux-session))
    (eat-self-input 1 'return)))

;;;###autoload
(defun eat-tmux-toggle (arg)
  (interactive "P")
  (require 'eat)
  (let ((buffer (window-buffer (selected-window))))
    (if (eq #'eat-mode (with-current-buffer buffer major-mode))
        (bury-buffer)
      (progn
        (with-current-buffer (eat)
          (eat-tmux-open arg))))))


(provide 'zsxh-tmux)

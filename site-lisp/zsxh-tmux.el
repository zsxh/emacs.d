;;; zsxh-tmux.el --- Seamless tmux session and window management from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL:
;; Version: 0.1.0
;; Package-Requires: ((emacs "30"))

;; This file is not part of GNU Emacs.

;;; Commentary:
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

(defun tmux--create-session (&optional name)
  "Create new tmux session with NAME."
  (let ((cmd "tmux new -d -P -F '#{session_name}'"))
    (unless (tmux--string-empty-p name)
      (setq cmd (concat cmd (format " -s '%s'" name))))
    ;; (message "%s" cmd)
    (string-trim (shell-command-to-string cmd))))

(defun tmux--create-window (&optional session dir name)
  "Create new tmux window with NAME in SESSION,
setting working-dircotry to DIR."
  (let ((cmd "tmux new-window -P -F '#{session_name}:#{window_id}'"))
    (unless (tmux--string-empty-p session)
      ;; -t <session_name>: 在最后加冒号保证 -t 后面加的是会话名而不是索引号
      (setq cmd (concat cmd (format " -t '%s:'" session))))
    (unless (tmux--string-empty-p dir)
      (setq cmd (concat cmd (format " -c '%s'" dir))))
    (unless (tmux--string-empty-p name)
      (setq cmd (concat cmd (format " -n '%s'" name))))
    ;; (message "%s" cmd)
    (string-trim (shell-command-to-string cmd))))

(defun tmux--select-or-create-session ()
  "Let user select existing tmux session or create new one."
  (let ((sessions (tmux--list-sessions)))
    (let ((choice (completing-read
                   "Select tmux session (or Create new): "
                   (append sessions '("Create New Session")) nil t)))
      (if (string= choice "Create New Session")
          (tmux--create-session
           (read-string "New tmux session name: " nil))
        choice))))

(defun tmux--select-or-create-window ()
  "Let user select existing tmux window or create new one in SESSION."
  (let* ((session (tmux--select-or-create-session))
         (windows (tmux--list-windows session)))
    (let ((choice (completing-read
                   "Select tmux window (or Create new): "
                   (append windows '("Create New Window")) nil t)))
      (if (string= choice "Create New Window")
          (tmux--create-window
           session
           (read-directory-name
            "Workwing Directory: "
            (or (project-root (project-current))
                default-directory))
           (read-string "New tmux window name: " nil))
        (format "%s:%s" session (substring choice 0 (string-search ":" choice)))))))

;;;###autoload
(defun tmux-run (session-window cmd)
  (interactive
   (list (tmux--select-or-create-window)
         (read-string "tmux run: ")))

  (unless (executable-find "tmux")
    (user-error "tmux not found in PATH"))

  (call-process "tmux" nil nil nil
                "send-keys"
                "-t" session-window
                cmd
                "C-m")
  (message "Run \"%s\" in tmux %s" cmd session-window))


(provide 'zsxh-tmux)

;; init-funcs.el --- Custom Functions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Custom Functions
;;

;;; Code:

(defun +funcs/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall 'text-mode)
    (setq buffer-offer-save t)
    $buf))

(defun +funcs/switch-empty-buffer-or-create (name)
  "Switch or create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing)."
  (let ((target-buffer (get-buffer name)))
    (if target-buffer
        (switch-to-buffer target-buffer)
      (+funcs/new-empty-buffer))))

(defun +funcs/switch-buffer-or-create (name)
  "Switch to the NAME buffer.
If the buffer doesn't exist, create a lisp-interaction buffer
and write the 'initial-scratch-message into it."
  (let* ((target-buffer-name name)
         (target-buffer (get-buffer target-buffer-name)))
    (unless target-buffer
      (setq target-buffer (get-buffer-create target-buffer-name))
      (with-current-buffer target-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer target-buffer)))

(defun keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (symbol)
                (and (boundp symbol)
                     (eq (symbol-value symbol) keymap)
                     ;; (not (eq symbol 'keymap))
                     (throw 'gotit symbol))))))

(defmacro +funcs/major-mode-leader-keys (mode-map &rest args)
  "Use general.el to define leader keys with both \"SPC m\" and \",\".
MODE-MAP is keymap symbol or literal keymap name, ARGS is the keybindings.
It returns a code string to define local leader keys."
  `(progn
     (general-define-key
      :states 'normal
      :keymaps (if (and (symbolp ,mode-map)
                        (keymapp (symbol-value ,mode-map)))
                   ,mode-map
                 (keymap-symbol ,mode-map))
      :major-modes t
      :prefix "SPC m"
      ,@args)
     (general-define-key
      :states 'normal
      :keymaps (if (and (symbolp ,mode-map)
                        (keymapp (symbol-value ,mode-map)))
                   ,mode-map
                 (keymap-symbol ,mode-map))
      :major-modes t
      :prefix ","
      ,@args)))

(defun +funcs/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun +funcs/sudo-edit-current-file ()
  "Sudo edit current file."
  (interactive)
  (cond ((eq major-mode 'dired-mode)
         (when-let ((filename (dired-file-name-at-point)))
           (find-file (concat "/sudo:root@localhost:" (expand-file-name filename)))))
        ((buffer-file-name)
         (let ((old-point (point)))
           (find-file (concat "/sudo:root@localhost:" (buffer-file-name)))
           (goto-char old-point)))))

(defun +funcs/sudo-shell-command (command)
  "Sudo execute string COMMAND in inferior shell; display output."
  (interactive (list (read-shell-command "Shell Command: ")))
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
                         " | sudo -S " command)))

(defun +funcs/shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02T14:38:04-07:00"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (delete-horizontal-space)
        (progn (delete-horizontal-space)
               (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun +funcs/narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
	      ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
	      ((equal major-mode 'org-mode) (org-narrow-to-subtree))
	      (t (error "Please select a region to narrow to"))))

;; Useful functions from xahlee's xah-fly-keys
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html
;; https://github.com/xahlee/xah-fly-keys
(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (setq $p4 (point))
    (delete-region $p3 $p4)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.
Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-20"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1                            ; whitespace begin
         $p2                            ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (delete-horizontal-space)
        (progn (delete-horizontal-space)
               (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun xah-copy-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
If in dired, copy the file/dir cursor is on, or marked files.
If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2018-06-18"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

(defun xah-display-minor-mode-key-priority ()
  "Print out minor mode's key priority.
URL `http://ergoemacs.org/emacs/minor_mode_key_priority.html'
Version 2017-01-27"
  (interactive)
  (mapc
   (lambda (x) (prin1 (car x)) (terpri))
   minor-mode-map-alist))

(defmacro add-hook-run-once (hook function &optional append local)
  "Like add-hook, but remove the hook after it is called"
  (let ((sym (gensym)))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (funcall ,function))
       (add-hook ,hook ',sym ,append ,local))))

(defun advice-run-once (symbol where function &optional props)
  (advice-add symbol :after (lambda (&rest _) (advice-remove symbol function)))
  (advice-add symbol where function props))

(defun +funcs/switch-to-buffer-dwim ()
  (interactive)
  (cond ((and (or (not (fboundp 'tramp-tramp-file-p))
                  (not (tramp-tramp-file-p default-directory)))
              (+project/root))
         (call-interactively 'project-switch-to-buffer))
        (t
         (call-interactively 'switch-to-buffer))))

(defun +funcs/toggle-maximize-buffer ()
  "Toggle Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (cl-remove-if
                           (lambda (window)
                             (window-parameter window 'no-delete-other-windows))
                           (window-list))))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;; https://with-emacs.com/posts/tips/quit-current-context/
;; Quit the minibuffer from any other window
(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        ;; (defining-kbd-macro
        ;;   (message
        ;;    (substitute-command-keys
        ;;     "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
        ;;   (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

;; https://emacs-china.org/t/gif/11887
(defun +funcs/video-compress-and-convert (video new)
  (interactive "fvideo path: \nfnew item name (eg. exam.mp4, exam.gif) : ")
  (let ((extension (cadr (split-string (file-name-nondirectory new) "\\."))))
    (if (string= extension "gif")
        (progn
          (shell-command
           (concat "ffmpeg -i " video " -r 5 " new))
          (message "%s convert to %s successfully!" video new))
      (progn
        (shell-command
         (concat "ffmpeg -i " video " -vcodec libx264 -b:v 5000k -minrate 5000k -maxrate 5000k -bufsize 4200k -preset fast -crf 20 -y -acodec libmp3lame -ab 128k " new))
        (message "%s compress and convert to %s successfully!" video new)))))

(defmacro +funcs/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun +funcs/pos-at-line-col (line column)
  "Transfer (`LINE', `COLUMN') to buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (move-to-column column)
    (point)))

(defun +funcs/try-get-major-mode-remap (mode)
  "Respect `major-mode-remap-alist', *-ts-mode for example."
  (alist-get mode major-mode-remap-alist mode))


(provide 'init-funcs)

;;; init-funcs.el ends here

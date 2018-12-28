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

(defmacro +funcs/set-leader-keys-for-major-mode (mode-map &rest args)
  "Use general.el to define leader keys with both \"SPC m\" and \",\".
Need major-mode-map symbol MODE-MAP and keybidngs map ARGS.

It returns a code string to define local leader keys."
  (defun prefix-m (element)
    (if (stringp element) (format "m%s" element) element))
  (let ((m-args (mapcar #'prefix-m args)))
    `(progn
       (general-define-key
        :states 'normal
        :keymaps ,mode-map
        :major-modes t
        :prefix "SPC"
        "m" '(nil :which-key "major")
        ,@m-args)
       (general-define-key
        :states 'normal
        :keymaps ,mode-map
        :major-modes t
        :prefix ","
        ,@args))))


(defun +funcs/sudo-edit-current-file ()
  "Sudo edit current file."
  (interactive)
  (when (buffer-file-name)
    (let ((old-point (point)))
      (find-file (concat "/sudo:root@localhost:" (buffer-file-name)))
      (goto-char old-point))))

(defun +funcs/sudo-shell-command (command)
  "Sudo execute string COMMAND in inferior shell; display output."
  (interactive (list (read-shell-command "Shell Command: ")))
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password: "))
                         " | sudo -S " command)))

(defun +funcs/shrink-whitespaces ()
  "Remove whitespaces around cursor to none."
  (interactive)
  (let* (
         ($p0 (point))
         $p1                            ; whitespace begin
         $p2                            ; whitespace end
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (delete-region $p1 $p2)))

;; Useful functions from xahlee's xah-fly-keys
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html
;; https://github.com/xahlee/xah-fly-keys

(defun xah-delete-current-text-block ()
  "Delete the current text block or selection, and copy to `kill-ring'.
A “block” is text between blank lines.
URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2017-07-09"
  (interactive)
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil "move")
            (progn (re-search-forward "\n[ \t]*\n+")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (setq $p2 (point))))
    (kill-region $p1 $p2)))

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

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


(provide 'init-funcs)

;;; init-funcs.el ends here

;; init-lisp-functions.el --- Custom Lisp Functions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Custom Lisp Functions
;;

;;; Code:

;; TODO: remove lispy
;; - [x] format(i)
;; - [x] structure delete(backspace)
;; - [x] eval(e)
;; - [x] structure select/mark(m)
;; - [x] lisp comment(;)
;; - [ ] slurp(>)
;; - [ ] barf(<)
;; - [x] clone(c)

(defun zsxh/empty-pair (&optional pos)
  (let* ((pos (or pos (point)))
         (char-begin (char-before pos))
         (char-end (char-after pos)))
    (cond
     ((and (eq ?\( char-begin) (eq ?\) char-end)) t)
     ((and (eq ?\[ char-begin) (eq ?\] char-end)) t)
     ((and (eq ?\" char-begin) (eq ?\" char-end)) t)
     ((and (eq ?\' char-begin) (eq ?\' char-end)) t)
     ((and (eq ?\{ char-begin) (eq ?\} char-end)) t)
     (t nil))))

(defun zsxh/in-string-or-comment ()
  (let* ((syntax (syntax-ppss))
         (in-string (nth 3 syntax))
         (in-comment (nth 4 syntax)))
    (or in-string in-comment)))

(defun zsxh/in-rest-blank-line (&optional pos)
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (= (save-excursion (skip-chars-forward " \t") (point))
         (save-excursion (end-of-line) (point))))))

(defun zsxh/end-of-line-without-breaking-sexp ()
  (let ((cur-line (line-number-at-pos))
        (last-pos -1))
    (ignore-errors
      (while (and (not (= last-pos (point)))
                  (= cur-line (line-number-at-pos)))
        (setq last-pos (point))
        (forward-sexp))
      (when (and (not (= cur-line (line-number-at-pos)))
                 (not (memq (char-before) '(?\) ?\]))))
        (previous-line)
        (end-of-line)))))

(defun zsxh/lisp-backward-delete-char (arg)
  "From \")|\", `delete-region', Otherwise `backward-delete-char-untabify'"
  (interactive "p")
  (let* ((syntax (syntax-ppss))
         (in-string (nth 3 syntax))
         (in-comment (nth 4 syntax))
         (in-string-or-comment (or in-string in-comment)))
    (cond
     ;; (...)|, [...]|, "..."|
     ((or (and (memq (char-before) '(?\) ?\])) (not in-string-or-comment))
          (and (eq ?\" (char-before)) (not in-string)))
      (backward-sexp)
      (mark-sexp)
      (call-interactively 'delete-region))
     ;; (|...), [|...], "|..."
     ((or (and (memq (char-before) '(?\( ?\[)) (not in-string-or-comment))
          (and (eq ?\" (char-before)) in-string))
      (backward-char 1)
      (mark-sexp)
      (call-interactively 'delete-region))
     ;; (|), [|], "|", '|', {|}
     ((zsxh/empty-pair)
      (delete-char 1)
      (backward-delete-char-untabify arg))
     (t (backward-delete-char-untabify arg)))))

(defun zsxh/lisp-eval-last-sexp ()
  "From \")|\", `eros-eval-last-sexp', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (eq ?\) (char-before)) (not (zsxh/in-string-or-comment)))
    (call-interactively 'eros-eval-last-sexp))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

(defun zsxh/lisp-format (&optional beg end)
  "From \")|\", `indent-region', Otherwise `self-insert-command'"
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (cond
   ((and beg end)
    (indent-region beg end))
   ((and (eq ?\) (char-before)) (not (zsxh/in-string-or-comment)))
    (indent-region (save-excursion (backward-sexp) (point)) (point)))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

(defun zsxh/lisp-mark-sexp ()
  "From \")|\", `mark-sexp', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (memq (char-after) '(?\( ?\[)) (not (zsxh/in-string-or-comment)))
    (mark-sexp))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

(defun zsxh/lisp-comment ()
  "From \"|(\", `comment-dwim', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((use-region-p)
    (call-interactively 'comment-dwim))
   ((zsxh/in-string-or-comment)
    (setq this-command 'self-insert-command)
    (call-interactively 'self-insert-command))
   ((not (zsxh/in-rest-blank-line))
    (ignore-errors
      (let ((char (save-excursion (skip-chars-forward " \t") (char-after))))
        (if (memq char '(?\( ?\[))
            (progn
              (mark-sexp)
              (call-interactively 'comment-dwim)
              (forward-sexp)
              (backward-sexp))
          (let ((beg (point))
                (end (save-excursion (zsxh/end-of-line-without-breaking-sexp) (point))))
            (if (not (= beg end))
                (progn
                  (push-mark beg)
                  (goto-char end)
                  (activate-mark)
                  (call-interactively 'comment-dwim))
              (newline)
              (previous-line)
              (end-of-line)
              (insert " ;; ")))))))
   (t (call-interactively 'comment-dwim))))

(defun zsxh/lisp-clone ()
  "From \")|\", clone, Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\])) (not (zsxh/in-string-or-comment)))
    (save-excursion
      (backward-sexp)
      (mark-sexp)
      (call-interactively 'kill-ring-save))
    (newline-and-indent)
    (yank))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

(bind-key (kbd "DEL") #'zsxh/lisp-backward-delete-char lispy-mode-map)
;; (bind-key (kbd "DEL") #'lispy-delete-backward lispy-mode-map)

(bind-key (kbd "e") #'zsxh/lisp-eval-last-sexp lispy-mode-map)
;; (bind-key (kbd "e") #'special-lispy-eval lispy-mode-map)

(bind-key (kbd "i") #'zsxh/lisp-format lispy-mode-map)
;; (bind-key (kbd "i") #'special-lispy-tab lispy-mode-map)

(bind-key (kbd "m") #'zsxh/lisp-mark-sexp lispy-mode-map)
;; (bind-key (kbd "m") #'special-lispy-mark-list lispy-mode-map)

(bind-key (kbd ";") #'zsxh/lisp-comment lispy-mode-map)
;; (bind-key (kbd ";") #'lispy-comment lispy-mode-map)

(bind-key (kbd "c") #'zsxh/lisp-clone lispy-mode-map)
;; (bind-key (kbd "c") #'special-lispy-clone lispy-mode-map)


(provide 'init-lisp-functions)


;;; init-lisp-functions.el ends here

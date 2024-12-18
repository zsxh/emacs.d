;; zsxh-lispy.el --- Custom Lispy Like Commands	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>

;; Commentary:
;;
;; Custom Lispy Like Commands
;;
;; Cheat Sheet:
;;
;; |---------------+-----+---------------+----------------------------------------|
;; | before        | key | after         | command                                |
;; |---------------+-----+---------------+----------------------------------------|
;; | (defun abc () | i   | (defun abc () | `zsxh-lispy/lisp-format'               |
;; | nil)*         |     |   nil)        |                                        |
;; |---------------+-----+---------------+----------------------------------------|
;; | (foo (bar)*)  | DEL | (foo *)       | `zsxh-lispy/lisp-backward-delete-char' |
;; |---------------+-----+---------------+----------------------------------------|
;; | (+ 1 1)*      | e   | (+ 1 1)* => 2 | `zsxh-lispy/lisp-eval-last-sexp'       |
;; |---------------+-----+---------------+----------------------------------------|
;; | *(foo)        | m   | mark "(foo)"  | `zsxh-lispy/lisp-mark-sexp'            |
;; |---------------+-----+---------------+----------------------------------------|
;; | *(foo)        | ;   | ;; (foo)      | `zsxh-lispy/lisp-comment'              |
;; |---------------+-----+---------------+----------------------------------------|
;; | (foo)*        | c   | (foo)         | `zsxh-lispy/lisp-clone'                |
;; |               |     | (foo)*        |                                        |
;; |---------------+-----+---------------+----------------------------------------|
;; | (foo)*bar     | >   | (foo bar)*    | `zsxh-lispy/lisp-slurp'                |
;; |---------------+-----+---------------+----------------------------------------|
;; | (foo bar)*    | <   | (foo)*bar     | `zsxh-lispy/lisp-barf'                 |
;; |---------------+-----+---------------+----------------------------------------|
;; | ((foo)*)      | )   | ((foo))*      | `zsxh-lispy/lisp-up-list'              |
;; |---------------+-----+---------------+----------------------------------------|
;;

;;; Code:


;;; Custom
(defgroup zsxh-lispy nil
  "Zsxh lispy-like Commands."
  :group 'zsxh-lispy)

;;; Utils
(defun zsxh-lispy/in-empty-pair-p (&optional pos)
  "Check if the current position or POS is inside an empty pair of parentheses, brackets, or quotes.

An empty pair is defined as one of the following: (),[],\"\",'',{}

If POS is provided, it checks the position at POS instead of the current point.

Returns t if the position is inside an empty pair, otherwise nil."
  (let* ((pos (or pos (point)))
         (char-begin (char-before pos))
         (char-end (char-after pos)))
    (member (cons char-begin char-end)
            '((?\( . ?\))
              (?\[ . ?\])
              (?\" . ?\")
              (?\' . ?\')
              (?\{ . ?\})))))

(defun zsxh-lispy/in-string-p (&optional pos)
  "Check if the current position or POS is inside a string

If POS is provided, it checks the position at POS instead of the current point.

Returns t if the position is inside a string, otherwise nil."
  (let* ((syntax (save-excursion (syntax-ppss (or pos (point)))))
         (in-string (nth 3 syntax)))
    in-string))

(defun zsxh-lispy/in-comment-p (&optional pos)
  "Check if the current position or POS is inside a comment

If POS is provided, it checks the position at POS instead of the current point.

Returns t if the position is inside a comment, otherwise nil."
  (let* ((syntax (save-excursion (syntax-ppss (or pos (point)))))
         (in-comment (nth 4 syntax)))
    in-comment))

(defun zsxh-lispy/in-string-or-comment-p (&optional pos)
    "Check if the current position or POS is inside a string or comment.

If POS is provided, it checks the position at POS instead of the current point.

Returns t if the position is inside a string or comment, otherwise nil."
  (let* ((syntax (save-excursion (syntax-ppss (or pos (point)))))
         (in-string (nth 3 syntax))
         (in-comment (nth 4 syntax)))
    (or in-string in-comment)))

(defun zsxh-lispy/line-trailing-blank-p (&optional pos)
  "Check if the current line is blank (contains only spaces or tabs) from the current position or POS.

If POS is provided, it checks the position at POS instead of the current point.

Returns t if the line is blank, otherwise nil."
  (save-excursion
    (goto-char (or pos (point)))
    (= (save-excursion (skip-chars-forward " \t") (point))
       (line-end-position))))

(defun zsxh-lispy/line-trailing-comment-p (&optional pos)
  "Check if the current line has a trailing comment starting from the current position or POS.

If POS is provided, it checks the position at POS instead of the current point.

Returns t if the line has a trailing comment, otherwise nil."
  (save-excursion
    (goto-char (or pos (line-beginning-position)))
    (skip-chars-forward " \t")
    (eq ?\; (char-after (point)))))

(defun zsxh-lispy/end-of-line-without-breaking-sexp ()
  "Move point to end-of-line position,
but if end-of-line point is in () or [],
then move point to closest closed ) or ].

This function ensures that the point does not end up inside a sexp
when moving to the end of the line. If the end of the line is inside
a sexp, it moves the point to the closest closing parenthesis or
bracket before the end of the line."
  (let ((cur-line (line-number-at-pos))
        (last-pos -1)
        (eol-pos (line-end-position))
        (cur-pos (point)))
    (ignore-errors
      (while (and (not (= last-pos cur-pos))
                  (not (= eol-pos cur-pos))
                  (= cur-line (line-number-at-pos)))
        (setq last-pos cur-pos)
        (if (zsxh-lispy/line-trailing-comment-p cur-pos)
            (end-of-line)
          (forward-sexp 1 t))
        (setq cur-pos (point)))
      (when (and (not (= cur-line (line-number-at-pos)))
                 (not (memq (char-before) '(?\) ?\]))))
        (previous-line)
        (end-of-line)))))

;;; Interactive Commands
;;;###autoload
(defun zsxh-lispy/lisp-backward-delete-char (arg)
  "Delete characters backward with special handling for sexps.

If the point is at the end of a sexp (e.g., \")|\"), it will delete the entire sexp.
Otherwise, it will behave like `backward-delete-char-untabify'.

ARG is the prefix argument, typically passed by interactive calls."
  (interactive "p")
  (let* ((syntax (syntax-ppss))
         (in-string (nth 3 syntax))
         (in-comment (nth 4 syntax))
         (in-string-or-comment (or in-string in-comment)))
    (cond
     ;; (...)|, [...]|, "..."|
     ((or (and (memq (char-before) '(?\) ?\])) (not in-string-or-comment))
          (and (eq ?\" (char-before)) (not in-string)))
      (backward-sexp 1 t)
      (mark-sexp)
      (call-interactively 'delete-region))
     ;; (|...), [|...], "|..."
     ((or (and (memq (char-before) '(?\( ?\[)) (not in-string-or-comment))
          (and (eq ?\" (char-before)) in-string))
      (backward-char 1)
      (mark-sexp)
      (call-interactively 'delete-region))
     ;; (|), [|], "|", '|', {|}
     ((zsxh-lispy/in-empty-pair-p)
      (delete-char 1)
      (backward-delete-char-untabify arg))
     (t (backward-delete-char-untabify arg)))))

;;;###autoload
(defun zsxh-lispy/lisp-eval-last-sexp ()
  "Evaluate the last s-expression.

If the point is at the end of a s-expression (e.g., \")|\"), it will evaluate
the s-expression using `eros-eval-last-sexp' or `cider-eval-last-sexp' if in
Clojure mode. Otherwise, it will behave like `self-insert-command'.

This function is designed to be used interactively and is particularly useful
for quickly evaluating s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((and (eq ?\) (char-before)) (not (zsxh-lispy/in-string-or-comment-p)))
    (if (and (derived-mode-p '(clojure-mode clojure-ts-mode))
             (functionp 'cider-eval-last-sexp))
        (call-interactively 'cider-eval-last-sexp)
      (call-interactively 'eros-eval-last-sexp)))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-format-region (start end)
  "Format the selected region.

This function formats the region between START and END by performing the following operations:

1. Remove extra spaces within the region.
2. Compact opened parentheses and brackets by removing spaces between them.
3. Compact closed parentheses and brackets by removing spaces before them.
4. Remove trailing spaces at the end of lines.
5. Indent the region after formatting.

START and END are the boundaries of the region to be formatted."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    ;; Remove extra spaces
    (while (re-search-forward "\s+" nil t)
      (unless (zsxh-lispy/in-string-or-comment-p)
        (replace-match " ")))
    ;; Compact opened parens
    (goto-char (point-min))
    (while (re-search-forward "\\([([]\\)[\s\n]+" nil t)
      (let* ((match-info (match-data))
             (match-start (nth 0 match-info)))
        (unless (zsxh-lispy/in-string-or-comment-p match-start)
          (if (save-excursion
                (skip-chars-backward "\s\n([")
                (and (eq (char-before (point)) ?\\)
                     (eq (char-before (1- (point))) ?\?)))
              ;; handle ?\(, ?\[, eg: '(?\( ?\))
              (replace-match "\\1 ")
            (replace-match "\\1"))
          (backward-char))))
    ;; Compact closed parens
    (goto-char (point-min))
    (while (re-search-forward "[\s\n]+\\([])]\\)" nil t)
      (let* ((match-info (match-data))
             (match-start (nth 0 match-info)))
        (unless (zsxh-lispy/in-string-or-comment-p match-start)
          (replace-match "\\1")
          (backward-char))))
    ;; Remove line trailing spaces
    (goto-char (point-min))
    (while (re-search-forward "\s+$" nil t)
      (unless (zsxh-lispy/in-string-p) ; not in string
        (replace-match "")))
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun zsxh-lispy/lisp-format (&optional beg end)
  "Format the current s-expression or region.

If a region is active, format the region between BEG and END.
If the point is at the end of an s-expression, format that s-expression.
Otherwise, behave like `self-insert-command'.

BEG and END are the boundaries of the region to be formatted."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (cond
   ((and beg end)
    (save-excursion (zsxh-lispy/format-region beg end)))
   ((and (memq (char-before) '(?\) ?\])) (not (zsxh-lispy/in-string-or-comment-p)))
    (save-excursion
      (zsxh-lispy/lisp-format-region
       (save-excursion (backward-sexp 1 t) (point))
       (point))))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-mark-sexp ()
  "Mark the current s-expression.

If the point is at the beginning of an s-expression (e.g., \"|(\"), it will mark
the entire s-expression using `mark-sexp'. Otherwise, it will behave like
`self-insert-command'.

This function is designed to be used interactively and is particularly useful
for quickly marking s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((and (memq (char-after) '(?\( ?\[))
         (not (zsxh-lispy/in-string-or-comment-p)))
    (mark-sexp))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-comment ()
  "Comment or uncomment the current s-expression or region.

If a region is active, it will call `comment-dwim' on the region.
If the point is at the beginning of an s-expression (e.g., \"|(\"), it will mark
the entire s-expression and call `comment-dwim'.
If the current line is not blank, it will comment the line or the s-expression at the point.
Otherwise, it will behave like `self-insert-command'.

This function is designed to be used interactively and is particularly useful
for quickly commenting s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((use-region-p)
    (call-interactively 'comment-dwim))
   ((zsxh-lispy/in-string-or-comment-p)
    (setq this-command 'self-insert-command)
    (call-interactively 'self-insert-command))
   ((not (zsxh-lispy/line-trailing-blank-p))
    (ignore-errors
      (let ((char (save-excursion (skip-chars-forward " \t") (char-after))))
        (if (memq char '(?\( ?\[))
            (progn
              ;; In interactive mode, `mark-sexp' sets the mark and activates the mark ring, and may trigger visual feedback (such as highlighting the region).
              ;; In batch mode, these interactive behaviors do not occur because batch mode is non-interactive and cannot display highlights or activate the mark ring.
              ;; If `mark-sexp' relies on certain interactive behaviors (like activating the mark ring), you can manually simulate these behaviors in batch mode.
              (push-mark (point))
              (mark-sexp)
              (activate-mark)
              (call-interactively 'comment-dwim)
              (forward-sexp 1 t)
              (backward-sexp 1 t))
          (let ((beg (point))
                (end (save-excursion (zsxh-lispy/end-of-line-without-breaking-sexp) (point))))
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

;;;###autoload
(defun zsxh-lispy/lisp-clone ()
  "Clone the current s-expression.

If the point is at the end of an s-expression (e.g., \")|\"), it will duplicate
the s-expression and place it on a new line. Otherwise, it will behave like
`self-insert-command'.

This function is designed to be used interactively and is particularly useful
for quickly duplicating s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\])) (not (zsxh-lispy/in-string-or-comment-p)))
    (let* ((beg (save-excursion (backward-sexp 1 t) (point)))
           (end (point))
           (str (buffer-substring-no-properties beg end)))
      (newline-and-indent)
      (insert str)))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-slurp ()
  "Slurp the next s-expression into the current one.

If the point is at the end of an s-expression (e.g., \")|\"), it will move the
next s-expression into the current one, adjusting spacing and indentation as
needed. Otherwise, it will behave like `self-insert-command'.

This function is designed to be used interactively and is particularly useful
for quickly modifying s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\]))
         (not (zsxh-lispy/in-string-or-comment-p)))
    (let* ((del-beg (point))
           (copy-beg (save-excursion (skip-chars-forward " \t") (point)))
           (end (save-excursion (forward-sexp 1 t) (point)))
           (str (buffer-substring-no-properties copy-beg end)))
      (when (length> str 0)
        (save-excursion
          (delete-region del-beg end)
          (backward-char)
          (unless (or (memq (char-before) '(?\( ?\[))
                      (string-prefix-p "\n" str))
            (insert " "))
          (insert str)
          (forward-char)
          (backward-sexp 1 t)
          (indent-sexp)))))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-barf ()
  "Barf the last s-expression out of the current one.

If the point is at the end of an s-expression (e.g., \")|\"), it will move the
last s-expression out of the current one, adjusting spacing and indentation as
needed. Otherwise, it will behave like `self-insert-command'.

This function is designed to be used interactively and is particularly useful
for quickly modifying s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\]))
         (not (zsxh-lispy/in-string-or-comment-p)))
    (save-excursion
      (backward-char)
      (let* ((beg (save-excursion (backward-sexp 1 t) (point)))
             (end (point))
             (str (buffer-substring-no-properties beg end))
             (line-num (line-number-at-pos)))
        (when (length> str 0)
          (delete-region beg end)
          (delete-region (save-excursion (skip-chars-backward " \t\n") (point)) beg)
          (when (zsxh-lispy/in-comment-p)
            (newline-and-indent))
          (forward-char)
          (save-excursion (insert " " str))
          (backward-sexp 1 t)
          (indent-sexp)))))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-up-list ()
  "Move the point up one level of parentheses or brackets.

If the point is at the end of an s-expression (e.g., \")|\"), it will move the
point up one level of parentheses or brackets using `up-list'. Otherwise, it
will behave like `self-insert-command'.

This function is designed to be used interactively and is particularly useful
for navigating nested s-expressions in Lisp-like languages."
  (interactive)
  (cond
   ((and (eq ?\) (char-before))
         (not (zsxh-lispy/in-string-or-comment-p)))
    (ignore-errors (up-list)))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;; Keymaps
;;;###autoload
(defvar zsxh-lispy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'zsxh-lispy/lisp-backward-delete-char)
    (define-key map (kbd "e") #'zsxh-lispy/lisp-eval-last-sexp)
    (define-key map (kbd "i") #'zsxh-lispy/lisp-format)
    (define-key map (kbd "m") #'zsxh-lispy/lisp-mark-sexp)
    (define-key map (kbd ";") #'zsxh-lispy/lisp-comment)
    (define-key map (kbd "c") #'zsxh-lispy/lisp-clone)
    (define-key map (kbd ">") #'zsxh-lispy/lisp-slurp)
    (define-key map (kbd "<") #'zsxh-lispy/lisp-barf)
    (define-key map (kbd ")") #'zsxh-lispy/lisp-up-list)
    map))

;;; Minor Mode
;;;###autoload
(define-minor-mode zsxh-lispy-mode
  "Toggles zsxh-lispy-mode."
  :global nil
  :group 'zsxh-lispy
  :keymap zsxh-lispy-map)


(provide 'zsxh-lispy)


;;; init-zsxh-lispy.el ends here

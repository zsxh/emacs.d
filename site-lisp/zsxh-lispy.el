;; zsxh-lispy.el --- Custom Lispy Like Commands	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh-lispy/emacs.d

;; Commentary:
;;
;; Custom Lispy Like Commands
;;
;; TODO: clojure cider eval last sexp
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
(defun zsxh-lispy/empty-pair (&optional pos)
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

(defun zsxh-lispy/in-string-or-comment ()
  (let* ((syntax (syntax-ppss))
         (in-string (nth 3 syntax))
         (in-comment (nth 4 syntax)))
    (or in-string in-comment)))

(defun zsxh-lispy/in-rest-blank-line (&optional pos)
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (= (save-excursion (skip-chars-forward " \t") (point))
         (save-excursion (end-of-line) (point))))))

(defun zsxh-lispy/end-of-line-without-breaking-sexp ()
  (let ((cur-line (line-number-at-pos))
        (last-pos -1))
    (ignore-errors
      (while (and (not (= last-pos (point)))
                  (= cur-line (line-number-at-pos)))
        (setq last-pos (point))
        (forward-sexp 1 t))
      (when (and (not (= cur-line (line-number-at-pos)))
                 (not (memq (char-before) '(?\) ?\]))))
        (previous-line)
        (end-of-line)))))

;;; Interactive Commands
;;;###autoload
(defun zsxh-lispy/lisp-backward-delete-char (arg)
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
     ((zsxh-lispy/empty-pair)
      (delete-char 1)
      (backward-delete-char-untabify arg))
     (t (backward-delete-char-untabify arg)))))

;;;###autoload
(defun zsxh-lispy/lisp-eval-last-sexp ()
  "From \")|\", `eros-eval-last-sexp', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (eq ?\) (char-before)) (not (zsxh-lispy/in-string-or-comment)))
    (call-interactively 'eros-eval-last-sexp))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-format (&optional beg end)
  "From \")|\", `indent-region', Otherwise `self-insert-command'"
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (cond
   ((and beg end)
    (indent-region beg end))
   ((and (eq ?\) (char-before)) (not (zsxh-lispy/in-string-or-comment)))
    (indent-region (save-excursion (backward-sexp 1 t) (point)) (point)))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-mark-sexp ()
  "From \")|\", `mark-sexp', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (memq (char-after) '(?\( ?\[)) (not (zsxh-lispy/in-string-or-comment)))
    (mark-sexp))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-comment ()
  "From \"|(\", `comment-dwim', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((use-region-p)
    (call-interactively 'comment-dwim))
   ((zsxh-lispy/in-string-or-comment)
    (setq this-command 'self-insert-command)
    (call-interactively 'self-insert-command))
   ((not (zsxh-lispy/in-rest-blank-line))
    (ignore-errors
      (let ((char (save-excursion (skip-chars-forward " \t") (char-after))))
        (if (memq char '(?\( ?\[))
            (progn
              (mark-sexp)
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
  "From \")|\", clone, Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\])) (not (zsxh-lispy/in-string-or-comment)))
    (let* ((beg (save-excursion (backward-sexp 1 t) (point)))
           (end (point))
           (str (buffer-substring-no-properties beg end)))
      (newline-and-indent)
      (insert str)))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-slurp ()
  "From \")|\", slurp, Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\])) (not (zsxh-lispy/in-string-or-comment)))
    (let* ((del-beg (point))
           (copy-beg (save-excursion (skip-chars-forward " \t") (point)))
           (end (save-excursion (forward-sexp 1 t) (point)))
           (str (buffer-substring-no-properties copy-beg end)))
      (when (length> str 0)
        (save-excursion
          (delete-region del-beg end)
          (backward-char)
          (insert " " str)
          (forward-char)
          (backward-sexp 1 t)
          (indent-sexp)))))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-barf ()
  "From \")|\", barf, Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (memq (char-before) '(?\) ?\])) (not (zsxh-lispy/in-string-or-comment)))
    (save-excursion
      (backward-char)
      (let* ((beg (save-excursion (backward-sexp 1 t) (point)))
             (end (point))
             (str (buffer-substring-no-properties beg end)))
        (when (length> str 0)
          (delete-region beg end)
          (delete-region (save-excursion (skip-chars-backward " \t\n") (point)) beg)
          (forward-char)
          (save-excursion (insert " " str))
          (backward-sexp 1 t)
          (indent-sexp)))))
   (t (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command))))

;;;###autoload
(defun zsxh-lispy/lisp-up-list ()
  "From \")|\", `up-list', Otherwise `self-insert-command'"
  (interactive)
  (cond
   ((and (eq ?\) (char-before)) (not (zsxh-lispy/in-string-or-comment)))
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

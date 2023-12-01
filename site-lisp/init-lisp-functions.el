;; init-lisp-functions.el --- Custom Lisp Functions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Custom Lisp Functions
;;

;;; Code:

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

(defun zsxh/lisp-backward-delete-char (arg)
  "From \")|\", `backward-kill-sexp', Otherwise `backward-delete-char-untabify'"
  (interactive "p")
  (let* ((syntax (syntax-ppss))
         (in-string (nth 3 syntax))
         (in-comment (nth 4 syntax))
         (in-string-or-comment (or in-string in-comment)))
    (cond
     ;; (...)|, [...]|, "..."|
     ((or (and (memq (char-before) '(?\) ?\])) (not in-string-or-comment))
          (and (eq ?\" (char-before)) (not in-string)))
      (backward-kill-sexp arg))
     ;; (|...), [|...]|, "|..."
     ((or (and (memq (char-before) '(?\( ?\[)) (not in-string-or-comment))
          (and (eq ?\" (char-before)) in-string))
      (backward-char 1)
      (kill-sexp arg))
     ;; (|), [|], "|", '|', {|}
     ((zsxh/empty-pair)
      (delete-char 1)
      (backward-delete-char-untabify arg))
     (t (backward-delete-char-untabify arg)))))

(bind-key (kbd "DEL") #'zsxh/lisp-backward-delete-char lispy-mode-map)
;; (bind-key (kbd "DEL") #'lispy-delete-backward lispy-mode-map)

;; TODO: slurp, barf, `forward-sexp', `kill-sexp', `yank'

(provide 'init-lisp-functions)

;;; init-lisp-functions.el ends here

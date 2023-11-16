;; init-lang-emacs-lisp.el --- Initialize Emacs Lisp Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Lisp configurations
;;

;;; Code:

;; NOTE: Elisp Guide - https://github.com/chrisdone/elisp-guide
;; Programmers who are too busy to read through long tutorials and manuals, but who want to extend their editor.
;; You don't need to learn everything from the ground up, just enough knowledge to be self-sufficient.
;; You've been using Emacs for a while and now it's time you started making some handy extensions for yourself.

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :defer t
  :bind ((:map emacs-lisp-mode-map
               ("C-c C-z" . ielm)
               ("C-c C-c" . eval-defun)
               ("C-c C-b" . eval-buffer)
               ("C-c C-:" . pp-eval-expression)
               ("C-c C-d" . edebug-defun)))
  :config
  ;; Note: '(emacs-lisp-mode-map) or (list 'emacs-lisp-mode-map)
  (dolist (mode-map '(emacs-lisp-mode-map lisp-interaction-mode-map))
    (+funcs/major-mode-leader-keys
     mode-map
     "'" '(ielm :which-key "ielm")
     "e" '(nil :which-key "eval")
     "ed" '(eval-defun :which-key "eval-defun")
     "ep" '(lispy-eval-and-comment :which-key "lispy-eval-and-comment")
     "ee" '(pp-eval-last-sexp :which-key "eval-last-sexp")
     "ej" '(eval-print-last-sexp :which-key "eval-print-last-sexp")
     "d" '(nil :which-key "debug")
     "df" '(edebug-defun :which-key "edebug-defun")
     "D" '(lispy-describe-inline :which-key "lispy-describe-inline")
     "m" '(nil :which-key "macro")
     "mc" '(pp-macroexpand-last-sexp :which-key "macroexpand-last-sexp")
     "me" '(pp-macroexpand-expression :which-key "macroexpand-expression")
     "ms" '(macrostep-expand :which-key "macrostep-expand")
     "g" '(nil :which-key "goto")
     "gd" '(xref-find-definitions :which-key "xref-find-definitions")
     "gr" '(xref-find-references :which-key "xref-find-references")
     "R" '(xref-find-references-and-replace :which-key "xref-find-references-and-replace")))

  ;; Align indent keywords
  ;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
  ;; @see https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
  (defun my-lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let (#'(buffer-substring (point)
                                  (progn (forward-sexp 1) (point)))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (length> function 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))
  (setq lisp-indent-function #'my-lisp-indent-function))

;; Interactive macro expander
(use-package macrostep
  :bind ((:map emacs-lisp-mode-map
               ("C-c e" . macrostep-expand))
         (:map lisp-interaction-mode-map
               ("C-c e" . macrostep-expand)))
  :config
  (with-eval-after-load 'evil
    (evil-define-minor-mode-key 'normal 'macrostep-mode
      "q" 'macrostep-collapse)))

;; TODO: remove lispy (format(i), sturcture delete(backspace), eval(e), structure select/mark(m), lisp comment(;), slurp(>), barf(<))
;; Short and sweet LISP editing
(use-package lispy
  :bind ((:map lispy-mode-map
          (":" . self-insert-command)
          ("C-j" . newline-and-indent)))
  :hook ((lisp-data-mode clojure-mode) . lispy-mode)
  :config
  ;; this requires CIDER or cider--display-interactive-eval-result function
  (setq lispy-eval-display-style 'overlay)
  (with-eval-after-load 'cider
    (defun lispy-eval (arg &optional e-str)
      "Eval the current sexp and display the result.
When ARG is 2, insert the result as a comment.
When at an outline, eval the outline."
      (interactive "p")
      (setq lispy-eval-output nil)
      (condition-case e
          (cond ((eq arg 2)
                 (lispy-eval-and-comment))
                ((and (looking-at lispy-outline)
                      (looking-at lispy-outline-header))
                 (lispy-eval-outline))
                (t
                 (let ((res (lispy--eval e-str)))
                   (when (memq major-mode lispy-clojure-modes)
                     (setq res (lispy--clojure-pretty-string res)))
                   (when lispy-eval-output
                     (setq res (concat lispy-eval-output res)))
                   (cond ((eq lispy-eval-display-style 'message)
                          (lispy-message res))
                         ((or (fboundp 'cider--display-interactive-eval-result)
                              (require 'cider nil t))
                          (cider--display-interactive-eval-result
                           res 'value (cdr (lispy--bounds-dwim))))
                         ((or (fboundp 'eros--eval-overlay)
                              (require 'eros nil t))
                          (eros--eval-overlay
                           res (cdr (lispy--bounds-dwim))))
                         (t
                          (error "Please install CIDER >= 0.10 or eros to display overlay"))))))
        (eval-error
         (lispy-message (cdr e)))))))

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :commands (eros-eval-last-sexp eros-eval-defun eros--make-result-overlay)
  :init
  (global-set-key [remap eval-last-sexp] #'eros-eval-last-sexp)
  (global-set-key [remap eval-defun] #'eros-eval-defun)
  :config
  ;; Inline previous result and why you should edebug
  ;; https://xenodium.com/inline-previous-result-and-why-you-should-edebug/
  (with-eval-after-load 'edebug
    (define-advice edebug-previous-result (:around (_ &rest r) eros-advice)
      "Adviced `edebug-previous-result'."
      (eros--make-result-overlay edebug-previous-result
        :where (point)
        :duration eros-eval-result-duration))))

;; Extra font lock for emacs lisp
(use-package elispfl
  :vc (:url "https://github.com/cireu/elispfl" :rev :newest)
  :after elisp-mode
  :config
  (elispfl-mode))


(provide 'init-lang-emacs-lisp)

;;; init-lang-emacs-lisp.el ends here

;; zsxh-lispy-test.el --- Test zsxh-lispy	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Code:

;; NOTE https://scripter.co/quick-intro-to-emacs-lisp-regression-testing/

;; NOTE: Run Tests
;; M-x eval-expression RET (ert t) RET
;; or
;; emacs -batch -l ert -l ../zsxh-lispy.el -l zsxh-lispy-test.el -f ert-run-tests-batch-and-exit

(require 'ert)

(require 'zsxh-lispy)

;;; ************************************
;;; Test `zsxh-lispy/in-empty-pair-p'
;;; ************************************

(ert-deftest test-zsxh-lispy/in-empty-pair-p-empty-parens ()
  "Test empty parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "()")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair-p))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-empty-brackets ()
  "Test empty square brackets."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "[]")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair-p))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-empty-quotes ()
  "Test empty double quotes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"\"")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair-p))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-empty-single-quotes ()
  "Test empty single quotes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "''")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair-p))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-empty-braces ()
  "Test empty curly braces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "{}")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair-p))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-non-empty-parens ()
  "Test non-empty parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a)")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-non-empty-brackets ()
  "Test non-empty square brackets."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "[a]")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-non-empty-quotes ()
  "Test non-empty double quotes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"a\"")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-non-empty-single-quotes ()
  "Test non-empty single quotes."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'a'")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-non-empty-braces ()
  "Test non-empty curly braces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "{a}")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-cursor-outside ()
  "Test cursor outside parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "()")
    (goto-char 3)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-nested-parens ()
  "Test nested parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(()())")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair-p)))
    (goto-char 5)
    (should (zsxh-lispy/in-empty-pair-p))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-unmatched-parens ()
  "Test unmatched parentheses."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))

    (erase-buffer)
    (insert ")")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-p-other-chars ()
  "Test other characters."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "a")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))

    (erase-buffer)
    (insert " ")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))

    (erase-buffer)
    (insert "\n")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair-p)))))

;;; ************************************
;;; Test `zsxh-lispy/in-string-or-comment-p'
;;; ************************************

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-in-string ()
  "Test if the function returns t when inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string.\"")
    (goto-char 2)
    (should (zsxh-lispy/in-string-or-comment-p))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-in-comment ()
  "Test if the function returns t when inside a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment.")
    (goto-char 3)
    (should (zsxh-lispy/in-string-or-comment-p))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-not-in-string-or-comment ()
  "Test if the function returns nil when not inside a string or comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () nil)")
    (goto-char 2)
    (should (not (zsxh-lispy/in-string-or-comment-p)))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-at-end-of-string ()
  "Test if the function returns t when at the end of a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string.\"")
    (goto-char 18)
    (should (zsxh-lispy/in-string-or-comment-p))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-at-end-of-comment ()
  "Test if the function returns t when at the end of a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment.")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment-p))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-in-nested-string ()
  "Test if the function returns t when inside a nested string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string with \\\"nested\\\" string.\"")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment-p))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-in-nested-comment ()
  "Test if the function returns t when inside a nested comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment with ;; nested comment.")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment-p))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-at-start-of-string ()
  "Test if the function returns t when at the start of a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string.\"")
    (goto-char 1)
    (should (not (zsxh-lispy/in-string-or-comment-p)))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-at-start-of-comment ()
  "Test if the function returns t when at the start of a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment.")
    (goto-char 1)
    (should (not (zsxh-lispy/in-string-or-comment-p)))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-p-in-string-with-escaped-quote ()
  "Test if the function returns t when inside a string with an escaped quote."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string with \\\"escaped\\\" quote.\"")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment-p))))

;;; ************************************
;;; Test `zsxh-lispy/line-trailing-blank-p'
;;; ************************************

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-empty-line ()
  "Test when the rest of the line is empty."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-only-spaces ()
  "Test when the rest of the line contains only spaces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "    \n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-only-tabs ()
  "Test when the rest of the line contains only tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\t\t\t\n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-mixed-spaces-and-tabs ()
  "Test when the rest of the line contains a mix of spaces and tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert " \t \t \n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-non-blank-rest ()
  "Test when the rest of the line contains non-blank characters."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo bar\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-non-blank-rest-with-spaces ()
  "Test when the rest of the line contains non-blank characters with leading spaces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "    foo bar\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-non-blank-rest-with-tabs ()
  "Test when the rest of the line contains non-blank characters with leading tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\t\tfoo bar\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-blank-p))))

(ert-deftest test-zsxh-lispy/line-trailing-blank-p-non-blank-rest-with-mixed-spaces-and-tabs ()
  "Test when the rest of the line contains non-blank characters with mixed leading spaces and tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert " \t \tfoo bar\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-blank-p))))

;;; ************************************
;;; Test `zsxh-lispy/line-trailing-blank-p'
;;; ************************************
(ert-deftest test-zsxh-lispy/line-beginning-blank-p-empty-line ()
  "Test that an empty line is considered blank at the beginning."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-only-spaces ()
  "Test if line beginning is blank when it contains only spaces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "   foo")
    (goto-char (point-min))
    (should (zsxh-lispy/line-beginning-blank-p))
    (goto-char 4)
    (should (zsxh-lispy/line-beginning-blank-p))
    (goto-char 5)
    (should-not (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-only-tabs ()
  "Test if line beginning is blank when it contains only tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\t\tfoo")
    (goto-char (point-min))
    (should (zsxh-lispy/line-beginning-blank-p))
    (goto-char 3)
    (should (zsxh-lispy/line-beginning-blank-p))
    (goto-char 4)
    (should-not (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-mixed-spaces-and-tabs ()
  "Test if line beginning is blank when it contains mixed spaces and tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert " \t foo")
    (goto-char (point-min))
    (should (zsxh-lispy/line-beginning-blank-p))
    (goto-char 4)
    (should (zsxh-lispy/line-beginning-blank-p))
    (goto-char 5)
    (should-not (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-non-blank-beginning ()
  "Test if line beginning is blank when it starts with non-blank characters."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo bar")
    (goto-char (point-min))
    (should (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-non-blank-beginning-with-spaces ()
  "Test if line beginning is blank when it contains non-blank characters with spaces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo bar")
    (goto-char 5)
    (should-not (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-non-blank-beginning-with-tabs ()
  "Test if line beginning is blank when it contains non-blank characters with tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo\tbar")
    (goto-char 5)
    (should-not (zsxh-lispy/line-beginning-blank-p))))

(ert-deftest test-zsxh-lispy/line-beginning-blank-p-non-blank-beginning-with-mixed ()
  "Test if line beginning is blank when it contains non-blank characters with mixed spaces and tabs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo \tbar")
    (goto-char 6)
    (should-not (zsxh-lispy/line-beginning-blank-p))))

;;; ************************************
;;; Test `zsxh-lispy/line-trailing-comment-p'
;;; ************************************

(ert-deftest test-zsxh-lispy/line-trailing-comment-p-empty-line ()
  "Test that an empty line is not considered a comment line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-comment-p))))

(ert-deftest test-zsxh-lispy/line-trailing-comment-p-comment-line ()
  "Test that a line starting with `;` is considered a comment line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "; This is a comment\n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-trailing-comment-p))))

(ert-deftest test-zsxh-lispy/line-trailing-comment-p-code-line ()
  "Test that a line starting with code is not considered a comment line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo ()\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-comment-p))))

(ert-deftest test-zsxh-lispy/line-trailing-comment-p-indented-comment-line ()
  "Test that an indented comment line is considered a comment line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "  ; This is an indented comment\n")
    (goto-char (point-min))
    (should (zsxh-lispy/line-trailing-comment-p))))

(ert-deftest test-zsxh-lispy/line-trailing-comment-p-mixed-line ()
  "Test that a line with code and a comment is not considered a comment line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () ; comment\n")
    (goto-char (point-min))
    (should-not (zsxh-lispy/line-trailing-comment-p))))

;;; ***************************************************
;;; Test `zsxh-lispy/end-of-line-without-breaking-sexp'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-basic ()
  "Test moving to end of line without breaking sexp."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)\n(baz qux)")
    (goto-char (point-min))
    (forward-char 5) ;; Move to inside the first sexp
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 9)))) ;; Should be at the end of the first line

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-inside-sexp ()
  "Test moving to end of line when point is inside a sexp."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz))\n(qux)")
    (goto-char (point-min))
    (forward-char 6) ;; Move to inside the nested sexp
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 14)))) ;; Should be at the end of the first line, outside the nested sexp

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-empty-line ()
  "Test moving to end of line when the line is empty."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\n(foo bar)")
    (goto-char (point-min))
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 1)))) ;; Should be at the end of the empty line

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-multiple-sexps ()
  "Test moving to end of line when the line contains multiple sexps."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar) (baz qux)\n(quux)")
    (goto-char (point-min))
    (forward-char 5) ;; Move to inside the first sexp
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 9)))) ;; Should be at the end of the first line, outside all sexps

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-string ()
  "Test moving to end of line when the line contains a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"foo bar\"\n(baz qux)")
    (goto-char (point-min))
    (forward-char 5) ;; Move to inside the string
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 9)))) ;; Should be at the end of the first line, outside the string

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-comment ()
  "Test moving to end of line when the line contains a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; foo bar\n(baz qux)")
    (goto-char (point-min))
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 11)))) ;; Should be at the end of the first line, outside the comment

(ert-deftest test-zsxh-lispy/end-of-line-without-breaking-sexp-with-line-end-comment ()
  "Test moving to the end of the line without breaking a sexp, with a comment in line end."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo) ;; foo bar\n(bar)")
    (goto-char (point-min))
    (zsxh-lispy/end-of-line-without-breaking-sexp)
    (should (eq (point) 17)))) ;; Point should be at the end of "(foo) ;; foo bar"

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-backward-delete-char'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-backward-delete-char-at-end-of-sexp ()
  "Test deleting at the end of a sexp."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)")
    (goto-char (point-max))
    (zsxh-lispy/lisp-backward-delete-char 1)
    (should (equal (buffer-string) ""))))

(ert-deftest test-zsxh-lispy/lisp-backward-delete-char-at-start-of-sexp ()
  "Test deleting at the start of a sexp."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)")
    (goto-char 2)
    (zsxh-lispy/lisp-backward-delete-char 1)
    (should (equal (buffer-string) ""))))

(ert-deftest test-zsxh-lispy/lisp-backward-delete-char-in-empty-pair-p ()
  "Test deleting in an empty pair."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "()")
    (goto-char (point-max))
    (zsxh-lispy/lisp-backward-delete-char 1)
    (should (equal (buffer-string) ""))))

(ert-deftest test-zsxh-lispy/lisp-backward-delete-char-in-string ()
  "Test deleting in a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"foo\"")
    (goto-char 5)
    (zsxh-lispy/lisp-backward-delete-char 1)
    (should (equal (buffer-string) "\"fo\""))))

(ert-deftest test-zsxh-lispy/lisp-backward-delete-char-in-comment ()
  "Test deleting in a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; foo")
    (goto-char (point-max))
    (zsxh-lispy/lisp-backward-delete-char 1)
    (should (equal (buffer-string) ";; fo"))))

(ert-deftest test-zsxh-lispy/lisp-backward-delete-char-normal-delete ()
  "Test normal backward delete behavior."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo")
    (goto-char (point-max))
    (zsxh-lispy/lisp-backward-delete-char 1)
    (should (equal (buffer-string) "fo"))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-format-region'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-format-region-remove-extra-spaces ()
  "Test removing extra spaces within the region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo  bar  baz)")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "(foo bar baz)"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-compact-opened-parens ()
  "Test compacting opened parentheses and brackets by removing spaces between them."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(  foo  (  bar  baz  )  )")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "(foo (bar baz))"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-compact-closed-parens ()
  "Test compacting closed parentheses and brackets by removing spaces before them."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz)  )")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "(foo (bar baz))"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-remove-line-trailing-spaces ()
  "Test removing trailing spaces at the end of lines."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar baz)  \n(qux quux)\n")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "(foo bar baz)\n(qux quux)\n"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-collapse-consecutive-empty-lines ()
  "Test collapsing consecutive empty lines into a single empty line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)\n\n\n(baz qux)\n\n")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "(foo bar)\n\n\n(baz qux)\n\n"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-indent-region ()
  "Test indenting the region after formatting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo\nbar\nbaz)")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "(foo\n bar\n baz)"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-special-char ()
  "Test special char after formatting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'(?\\( ?\\[)")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "'(?\\( ?\\[)"))))

(ert-deftest test-zsxh-lispy/lisp-format-region-special-char-with-spaces ()
  "Test special char with spaces after formatting."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "'(?\\( \n ?\\[)")
    (zsxh-lispy/lisp-format-region (point-min) (point-max))
    (should (equal (buffer-string) "'(?\\( ?\\[)"))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-format'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-format-region ()
  "Test formatting a region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo  bar  (baz   qux))")
    (zsxh-lispy/lisp-format)
    (should (equal (buffer-string) "(foo bar (baz qux))"))))

(ert-deftest test-zsxh-lispy/lisp-format-self-insert ()
  "Test that =self-insert-command' is called when no region or s-expression is active."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo")
    (funcall-interactively 'zsxh-lispy/lisp-format)
    (should (eq this-command 'self-insert-command))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-format'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-mark-sexp-at-beginning-of-sexp ()
  "Test marking an s-expression when point is at the beginning."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)")
    (goto-char (point-min))
    (zsxh-lispy/lisp-mark-sexp)
    (should (equal (mark) (point-max)))))

(ert-deftest test-zsxh-lispy/lisp-mark-sexp-inside-string ()
  "Test behavior when point is inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"(foo bar)\"")
    (goto-char (point-min))
    (forward-char 1)
    (zsxh-lispy/lisp-mark-sexp)
    (should (equal this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-mark-sexp-inside-comment ()
  "Test behavior when point is inside a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "; (foo bar)")
    (goto-char (point-min))
    (forward-char 2)
    (zsxh-lispy/lisp-mark-sexp)
    (should (equal this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-mark-sexp-at-end-of-sexp ()
  "Test behavior when point is at the end of an s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)")
    (goto-char (point-max))
    (zsxh-lispy/lisp-mark-sexp)
    (should (equal this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-mark-sexp-with-nested-sexp ()
  "Test marking a nested s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz))")
    (goto-char (point-min))
    (forward-char 5)
    (zsxh-lispy/lisp-mark-sexp)
    (should (equal (mark) (+ (point) 9)))))

(ert-deftest test-zsxh-lispy/lisp-format-preserve-unquote-splicing-symbol ()
  "Test that formatting doesn't remove the `,@' unquote-splicing symbol."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "`(\"abc\" ,@(list 1 2 3))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-format)
    (should (equal (buffer-string) "`(\"abc\" ,@(list 1 2 3))"))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-comment'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-comment-region ()
  "Test commenting a region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo)\n(bar)")
    (goto-char (point-min))
    (push-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) ";; (foo)\n;; (bar)"))))

(ert-deftest test-zsxh-lispy/lisp-comment-sexp-at-beginning ()
  "Test commenting an s-expression at the beginning."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo)\n(bar)")
    (goto-char (point-min))
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) ";; (foo)\n(bar)"))))

(ert-deftest test-zsxh-lispy/lisp-comment-line-not-blank ()
  "Test commenting a line that is not blank."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo)\nbar")
    (goto-char (point-min))
    (forward-line)
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) "(foo)\n;; bar"))))

(ert-deftest test-zsxh-lispy/lisp-comment-self-insert ()
  "Test self-insert behavior when in a string or comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"foo\"")
    (goto-char (point-min))
    (forward-char)
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-comment-blank-line ()
  "Test commenting a blank line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\n")
    (goto-char (point-min))
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) ";; \n"))))

(ert-deftest test-zsxh-lispy/lisp-uncomment-region ()
  "Test uncommenting a region."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; (foo)\n;; (bar)")
    (goto-char (point-min))
    (push-mark (point))
    (goto-char (point-max))
    (activate-mark)
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) "(foo)\n(bar)"))))

(ert-deftest test-zsxh-lispy/lisp-uncomment-sexp-at-beginning ()
  "Test uncommenting an s-expression at the beginning."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; (foo)\n(bar)")
    (goto-char (point-min))
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) "(foo)\n(bar)"))))

(ert-deftest test-zsxh-lispy/lisp-uncomment-blank-line ()
  "Test uncommenting a blank line."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; \n")
    (goto-char (point-min))
    (call-interactively 'zsxh-lispy/lisp-comment)
    (should (equal (buffer-string) "\n"))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-clone'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-clone-at-end-of-sexp ()
  "Test cloning an s-expression when point is at the end."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar baz)")
    (goto-char (point-max))
    (zsxh-lispy/lisp-clone)
    (should (equal (buffer-string) "(foo bar baz)\n(foo bar baz)"))))

(ert-deftest test-zsxh-lispy/lisp-clone-at-beginning-of-sexp ()
  "Test cloning an s-expression when point is at the beginning."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar baz)")
    (goto-char (point-min))
    (zsxh-lispy/lisp-clone)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-clone-inside-sexp ()
  "Test cloning an s-expression when point is inside the s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar baz)")
    (goto-char 2)
    (zsxh-lispy/lisp-clone)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-clone-in-string ()
  "Test cloning when point is inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"foo bar baz\"")
    (goto-char 2)
    (zsxh-lispy/lisp-clone)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-clone-in-comment ()
  "Test cloning when point is inside a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; foo bar baz")
    (goto-char 2)
    (zsxh-lispy/lisp-clone)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-clone-with-empty-sexp ()
  "Test cloning an empty s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "()")
    (goto-char (point-max))
    (zsxh-lispy/lisp-clone)
    (should (equal (buffer-string) "()\n()"))))

(ert-deftest test-zsxh-lispy/lisp-clone-with-nested-sexp ()
  "Test cloning a nested s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-clone)
    (should (equal (buffer-string) "(foo (bar baz))\n(foo (bar baz))"))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-slurp'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-slurp-basic ()
  "Test basic slurping of an s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo)\n(bar)")
    (goto-char (point-min))
    (search-forward ")")
    (zsxh-lispy/lisp-slurp)
    (should (equal (buffer-string) "(foo\n (bar))"))))

(ert-deftest test-zsxh-lispy/lisp-slurp-with-space ()
  "Test slurping with space between s-expressions."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo) (bar)")
    (goto-char (point-min))
    (search-forward ")")
    (zsxh-lispy/lisp-slurp)
    (should (equal (buffer-string) "(foo (bar))"))))

(ert-deftest test-zsxh-lispy/lisp-slurp-nested ()
  "Test slurping a nested s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (baz))\n(bar)")
    (goto-char (point-min))
    (search-forward ")")
    (should-error (zsxh-lispy/lisp-slurp))
    (should (equal (buffer-string) "(foo (baz))\n(bar)"))))

(ert-deftest test-zsxh-lispy/lisp-slurp-self-insert ()
  "Test that `self-insert-command' is called when not at the end of an s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo)")
    (goto-char (point-min))
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-slurp-in-string ()
  "Test that `self-insert-command' is called when inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"(foo)\"")
    (goto-char (point-min))
    (search-forward ")")
    (zsxh-lispy/lisp-slurp)
    (should (eq this-command 'self-insert-command))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-barf'
;;; ***************************************************

(ert-deftest zsxh-lispy/lisp-barf-test-1 ()
  "Test barfing the last s-expression out of a list."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo) (bar baz)"))))

(ert-deftest zsxh-lispy/lisp-barf-test-2 ()
  "Test barfing the last s-expression out of a list with multiple nested elements."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar baz))")
    (goto-char (point-max))
    (backward-char 1)
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo (bar) baz)"))))

(ert-deftest zsxh-lispy/lisp-barf-test-3 ()
  "Test barfing the last s-expression out of a list with no nested elements."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo bar)")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo) bar"))))

(ert-deftest zsxh-lispy/lisp-barf-test-4 ()
  "Test barfing the last s-expression out of a list with extra spaces."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar  baz))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo) (bar  baz)"))))

(ert-deftest zsxh-lispy/lisp-barf-test-5 ()
  "Test barfing the last s-expression out of a list with comments."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar ; comment\nbaz))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (eq this-command 'self-insert-command))))

(ert-deftest zsxh-lispy/lisp-barf-test-6 ()
  "Test barfing the last s-expression out of a list with strings."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (\"bar\" baz))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo) (\"bar\" baz)"))))

(ert-deftest zsxh-lispy/lisp-barf-test-7 ()
  "Test barfing the last s-expression out of a list with no elements."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo ())")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo) ()"))))

(ert-deftest zsxh-lispy/lisp-barf-test-8 ()
  "Test barfing the last s-expression out of a list with multiple lines."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo\n(bar))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(foo) (bar)"))))

(ert-deftest zsxh-lispy/lisp-barf-test-9 ()
  "Test barfing the last s-expression with trailing comment"
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(lambda ()\n  ;; returning the used window.\n  (func1))")
    (goto-char (point-max))
    (zsxh-lispy/lisp-barf)
    (should (equal (buffer-string) "(lambda ()\n  ;; returning the used window.\n  ) (func1)"))))

;;; ***************************************************
;;; Test `zsxh-lispy/lisp-up-list'
;;; ***************************************************

(ert-deftest test-zsxh-lispy/lisp-up-list-at-end-of-sexp ()
  "Test moving up one level of parentheses when at the end of an s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar))")
    (goto-char 11)
    (call-interactively 'zsxh-lispy/lisp-up-list)
    (should (equal 12 (point)))))

(ert-deftest test-zsxh-lispy/lisp-up-list-inside-string ()
  "Test that the function does not move up when inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo \"(bar)\")")
    (goto-char 12)
    (call-interactively 'zsxh-lispy/lisp-up-list)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-up-list-at-beginning-of-sexp ()
  "Test that the function does not move up when at the beginning of an s-expression."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar))")
    (goto-char (point-min))
    (call-interactively 'zsxh-lispy/lisp-up-list)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-up-list-inside-comment ()
  "Test that the function does not move up when inside a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo ;(bar)\n)")
    (end-of-line)
    (call-interactively 'zsxh-lispy/lisp-up-list)
    (should (eq this-command 'self-insert-command))))

(ert-deftest test-zsxh-lispy/lisp-up-list-nested-sexp ()
  "Test moving up multiple levels of nested s-expressions."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo (bar (baz)))")
    (goto-char 16)
    (call-interactively 'zsxh-lispy/lisp-up-list)
    (should (equal 17 (point)))))


(provide 'zsxh-lispy-test)

;;; zsxh-lispy-test.el ends here

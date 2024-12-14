;; zsxh-lispy-test.el --- Test zsxh-lispy	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Code:

(require 'ert)

(require 'zsxh-lispy)

;;; ************************************
;;; Test `zsxh-lispy/in-empty-pair'
;;; ************************************

(ert-deftest test-zsxh-lispy/in-empty-pair-empty-parens ()
  "Test empty parentheses."
  (with-temp-buffer
    (insert "()")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair))))

(ert-deftest test-zsxh-lispy/in-empty-pair-empty-brackets ()
  "Test empty square brackets."
  (with-temp-buffer
    (insert "[]")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair))))

(ert-deftest test-zsxh-lispy/in-empty-pair-empty-quotes ()
  "Test empty double quotes."
  (with-temp-buffer
    (insert "\"\"")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair))))

(ert-deftest test-zsxh-lispy/in-empty-pair-empty-single-quotes ()
  "Test empty single quotes."
  (with-temp-buffer
    (insert "''")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair))))

(ert-deftest test-zsxh-lispy/in-empty-pair-empty-braces ()
  "Test empty curly braces."
  (with-temp-buffer
    (insert "{}")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))
    (goto-char 2)
    (should (zsxh-lispy/in-empty-pair))))

(ert-deftest test-zsxh-lispy/in-empty-pair-non-empty-parens ()
  "Test non-empty parentheses."
  (with-temp-buffer
    (insert "(a)")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-non-empty-brackets ()
  "Test non-empty square brackets."
  (with-temp-buffer
    (insert "[a]")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-non-empty-quotes ()
  "Test non-empty double quotes."
  (with-temp-buffer
    (insert "\"a\"")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-non-empty-single-quotes ()
  "Test non-empty single quotes."
  (with-temp-buffer
    (insert "'a'")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-non-empty-braces ()
  "Test non-empty curly braces."
  (with-temp-buffer
    (insert "{a}")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-cursor-outside ()
  "Test cursor outside parentheses."
  (with-temp-buffer
    (insert "()")
    (goto-char 3)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-nested-parens ()
  "Test nested parentheses."
  (with-temp-buffer
    (insert "(()())")
    (goto-char 2)
    (should (not (zsxh-lispy/in-empty-pair)))
    (goto-char 5)
    (should (zsxh-lispy/in-empty-pair))))

(ert-deftest test-zsxh-lispy/in-empty-pair-unmatched-parens ()
  "Test unmatched parentheses."
  (with-temp-buffer
    (insert "(")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair))))
  (with-temp-buffer
    (insert ")")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))))

(ert-deftest test-zsxh-lispy/in-empty-pair-other-chars ()
  "Test other characters."
  (with-temp-buffer
    (insert "a")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair))))
  (with-temp-buffer
    (insert " ")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair))))
  (with-temp-buffer
    (insert "\n")
    (goto-char 1)
    (should (not (zsxh-lispy/in-empty-pair)))))

;;; ************************************
;;; Test `zsxh-lispy/in-string-or-comment'
;;; ************************************

(ert-deftest test-zsxh-lispy/in-string-or-comment-in-string ()
  "Test if the function returns t when inside a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string.\"")
    (goto-char 2)
    (should (zsxh-lispy/in-string-or-comment))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-in-comment ()
  "Test if the function returns t when inside a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment.")
    (goto-char 3)
    (should (zsxh-lispy/in-string-or-comment))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-not-in-string-or-comment ()
  "Test if the function returns nil when not inside a string or comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo () nil)")
    (goto-char 2)
    (should (not (zsxh-lispy/in-string-or-comment)))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-at-end-of-string ()
  "Test if the function returns t when at the end of a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string.\"")
    (goto-char 18)
    (should (zsxh-lispy/in-string-or-comment))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-at-end-of-comment ()
  "Test if the function returns t when at the end of a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment.")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-in-nested-string ()
  "Test if the function returns t when inside a nested string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string with \\\"nested\\\" string.\"")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-in-nested-comment ()
  "Test if the function returns t when inside a nested comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment with ;; nested comment.")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-at-start-of-string ()
  "Test if the function returns t when at the start of a string."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string.\"")
    (goto-char 1)
    (should (not (zsxh-lispy/in-string-or-comment)))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-at-start-of-comment ()
  "Test if the function returns t when at the start of a comment."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; This is a comment.")
    (goto-char 1)
    (should (not (zsxh-lispy/in-string-or-comment)))))

(ert-deftest test-zsxh-lispy/in-string-or-comment-in-string-with-escaped-quote ()
  "Test if the function returns t when inside a string with an escaped quote."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "\"This is a string with \\\"escaped\\\" quote.\"")
    (goto-char 21)
    (should (zsxh-lispy/in-string-or-comment))))


(provide 'zsxh-lispy-test)

;;; zsxh-lispy-test.el ends here

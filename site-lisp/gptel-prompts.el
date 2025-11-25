;; gptel-prompts.el --- gptel prompts	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Custom prompts for gptel, stored as markdown files with YAML frontmatter.
;;  Each markdown file should contain:
;;  - YAML frontmatter with a :name field for the prompt name
;;  - The actual prompt content after the frontmatter
;;
;;  The prompts are loaded into gptel-directives and can be used with gptel.
;;

;;; Code:

(require 'gptel)
(require 'yaml)

(defvar gptel-prompts-dir (locate-user-emacs-file "site-lisp/gptel-prompts")
  "Directory containing gptel prompt markdown files.")

(defvar gptel-prompts-base-directives '((no-system-prompt . nil))
  "Base directives for gptel prompts.")

(defun gptel-prompts-read-file (file)
  "Read a prompt file and return a cons cell (PROMPT-NAME . PROMPT-STRING).
FILE should be a markdown file with YAML frontmatter containing a :name field.
The content after the frontmatter is used as the prompt string."
  (if (not (and (file-readable-p file)
                (file-regular-p file)))
      (prog1 nil
        (message "gptel-prompts: File %s is not parseable" file))
    (with-temp-buffer
      (insert-file-contents file)
      (unless (re-search-forward "^---[ \t]*$" nil t)
        (error "No opening delimiter '---' found in %s") file)
      (forward-line 1)
      (let ((meta-beg (point)))
        (unless (re-search-forward "^---[ \t]*$" nil t)
          (error "No closing delimiter '---' found in %s") file)
        (let* ((meta-end (match-beginning 0))
               (meta-str (buffer-substring-no-properties meta-beg meta-end))
               (prompt-str-beg (1+ (match-end 0)))
               (meta-yaml (yaml-parse-string
                           meta-str
                           :object-type 'plist
                           :object-key-type 'keyword
                           :sequence-type 'list))
               (prompt-name (plist-get meta-yaml :name))
               (prompt-str (buffer-substring-no-properties prompt-str-beg (point-max))))
          (cons (make-symbol prompt-name) prompt-str))))))

(defun gptel-prompts-refresh ()
  "Refresh gptel prompts by reading all markdown files in gptel-prompts directory.
Each markdown file should contain YAML frontmatter with a :name field.
The content after the frontmatter is used as the prompt string."
  (interactive)
  (let ((file-prompts nil)
        (files (cl-delete-if-not
                (lambda (file)
                  (and (file-regular-p file)
                       (string= (file-name-extension file) "md")))
                (directory-files gptel-prompts-dir t))))
    (dolist (file files)
      (push (gptel-prompts-read-file file) file-prompts))
    (sort file-prompts (lambda (a b)
                         (string< (downcase (symbol-name (car a)))
                                  (downcase (symbol-name (car b))))))
    (setq gptel-directives
          (append gptel-prompts-base-directives file-prompts))))


(provide 'gptel-prompts)

;;; gptel-prompts.el ends here

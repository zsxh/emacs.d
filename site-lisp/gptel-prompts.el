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

(defcustom gptel-prompts-system-placeholders
  '(("${DATE}" . (lambda () (format-time-string "%Y-%m-%d"))))
  "Alist of placeholder tokens to expand in the system prompt.

Each element is (PLACEHOLDER . FUNCTION), where PLACEHOLDER is a
literal string searched for in the system prompt and FUNCTION is a
thunk \(a zero-argument function) returning the replacement text to
insert.

Only the system prompt is affected, see
`gptel-prompts-inject-system-placeholders' for the expansion logic.")

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

;; NOTE: https://github.com/karthink/gptel/issues/481#issuecomment-3203716169
(defun gptel-prompts-inject-buffers (_)
  "Search backward, injecting text files into context as needed.

Include buffers by name as:

@buffer *scratch*"
  (while (and (re-search-backward "^\\s-*@buffer\\b" nil t) ;look for @buffer
              (not (get-char-property (point) 'gptel))) ;avoid LLM response regions
    (goto-char (match-end 0))
    (delete-region (point) (line-beginning-position))
    (let ((buf-name (string-trim
                     (buffer-substring-no-properties
                      (point) (line-end-position)))))
      (if (not (buffer-live-p (get-buffer buf-name)))
          (message "Buffer \"%s\" not live, ignoring @buffer"
                   buf-name)
        (delete-region (point) (line-end-position))
        (insert (format "\nIn buffer `%s`:\n\n```\n" buf-name))
        (insert-buffer-substring-no-properties buf-name)
        (insert "\n```\n")))))

(defun gptel-prompts-insert-buffer (&optional buffer-name)
  "Insert buffer name with @buffer prefix in current buffer.
When called interactively, prompts for buffer name with completion."
  (interactive
   (list (completing-read "Select buffer: "
                          (mapcar #'buffer-name
                                  (seq-filter
                                   (lambda (buf)
                                     (not (string-prefix-p " " (buffer-name buf))))
                                   (buffer-list)))
                          nil t nil nil (buffer-name))))
  (insert "\n@buffer " buffer-name "\n"))

(defun gptel-prompts-insert-file (&optional file-path)
  "Insert a markdown file link at point.
FILE-PATH: Path to the file to link to."
  (interactive
   (list (read-file-name "Select file: ")))
  (insert " [](" file-path ") "))

(defun gptel-prompts-insert (&optional type)
  "Insert file or buffer reference with appropriate prefix.
When called interactively, prompts for file or buffer type."
  (interactive
   (list (completing-read "File Or Buffer:  " '(" file" " buffer") nil t)))
  (cond
   ((string= type " file")
    (call-interactively #'gptel-prompts-insert-file))
   ((string= type " buffer")
    (call-interactively #'gptel-prompts-insert-buffer))))

(defun gptel-prompts-inject-system-placeholders (_)
  "Replace placeholder tokens in the system prompt with computed text.
This runs as a `gptel-prompt-transform-functions' entry.  The system
prompt lives in the buffer-local variable `gptel-system-prompt', not
in the buffer text.

Only the system message is affected:
- When `gptel-system-prompt' is a string, expand placeholders in it.
- When it is a list, only the first element (the system message) is
  expanded; the conversation template turns in the remaining elements
  are left untouched.
Conversation text in the prompt buffer is never modified.

Iterate over `gptel-prompts-system-placeholders' once.  Each token's
function is called at most once, only if the token is present."
  (when (and (boundp 'gptel-system-prompt) gptel-system-prompt)
    (let* ((listp (consp gptel-system-prompt))
           (sp (if listp (car gptel-system-prompt) gptel-system-prompt)))
      (when (stringp sp)
        (dolist (elt gptel-prompts-system-placeholders)
          (let ((ph (car elt)))
            (when (string-match-p ph sp)
              (setq sp (string-replace ph (funcall (cdr elt)) sp)))))
        (setq-local gptel-system-prompt
                    (if listp (cons sp (cdr gptel-system-prompt)) sp))))))


(provide 'gptel-prompts)

;;; gptel-prompts.el ends here

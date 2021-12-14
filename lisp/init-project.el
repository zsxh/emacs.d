;; init-project.el --- project configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  project configurations
;;

;;; Code:

;; NOTE: https://emacstalk.github.io/post/010/ EmacsTalk
;; NOTE: https://github.com/karthink/project-x
;; `project-remember-project', `project-forget-project',
;; `lsp-workspace-folders-add', `lsp-workspace-folders-remove'
(use-package project
  :ensure nil
  :defer t
  :commands (project-root project-forget-zombie-projects)
  :config
  ;; autoremove zombie projects
  (run-with-idle-timer 5 nil (lambda () (project-forget-zombie-projects)))

  (setq my/project-local-identifier '(".project" "go.mod" "Cargo.toml"
                                      "project.clj" "pom.xml" "package.json"
                                      "Makefile" "README.org" "README.md"))

  (defun my/project-try-local (dir)
    "Determine if DIR is a non-VC project."
    (if-let ((root (if (listp my/project-local-identifier)
                       (seq-some (lambda (n)
                                   (locate-dominating-file dir n))
                                 my/project-local-identifier)
                     (locate-dominating-file dir my/project-local-identifier))))
        (cons 'local root)))

  (cl-defmethod project-root ((project (head local)))
    (cdr project))

  (cl-defmethod project-root ((project (eql nil))) nil)

  (add-hook 'project-find-functions 'my/project-try-local)

  (when (executable-find "fd")
    (defvar my/project-prune-patterns
      '(;; VCS
        "*/.git"
        "*/.svn"
        "*/.cvs"
        "*/.tox"
        "*/.bzr"
        "*/.hg"
        "*/.DS_Store"
        "*/.sass-cache"
        "*/elpy"
        "*/dcache"
        "*/.npm"
        "*/.tmp"
        "*/.idea"
        "*/node_modules"
        "*/bower_components"
        "*/.gradle"
        "*/.cask")
      "Ignored directories(prune patterns).")

    (defvar my/project-ignore-filenames
      '(;; VCS
        ;; project misc
        "*.log"
        ;; Ctags
        "tags" "TAGS"
        ;; compressed
        "*.tgz" "*.gz" "*.xz" "*.zip" "*.tar" "*.rar"
        ;; Global/Cscope
        "GTAGS" "GPATH" "GRTAGS" "cscope.files"
        ;; html/javascript/css
        "*bundle.js" "*min.js" "*min.css"
        ;; Images
        "*.png" "*.jpg" "*.jpeg" "*.gif" "*.bmp" "*.tiff" "*.ico"
        ;; documents
        "*.doc" "*.docx" "*.xls" "*.ppt" "*.pdf" "*.odt"
        ;; C/C++
        "*.obj" "*.so" "*.o" "*.a" "*.ifso" "*.tbd" "*.dylib" "*.lib" "*.d" "*.dll" "*.exe"
        ;; Java
        ".metadata*" "*.class" "*.war" "*.jar"
        ;; Emacs/Vim
        "*flymake" "#*#" ".#*" "*.swp" "*~" "*.elc"
        ;; Python
        "*.pyc")
      "Ignored file names.  Wildcast is supported.")

    (setq my/fd-prune-patterns
          (mapconcat (lambda (p)
                       (format "-E \"%s\"" (replace-regexp-in-string "^\*/" "" p)))
                     my/project-prune-patterns " ")

          my/fd-ignore-filenames
          (mapconcat (lambda (p)
                       (format "-E \"%s\"" p))
                     my/project-ignore-filenames " "))

    (defun my/project-files-in-directory (dir)
      "Use `fd' to list files in DIR."
      (let* ((default-directory dir)
             (localdir (file-local-name (expand-file-name dir)))
             (command (format "fd -H -t f -0 %s %s . %s" my/fd-prune-patterns my/fd-ignore-filenames localdir)))
        (project--remote-file-names
         (sort (split-string (shell-command-to-string command) "\0" t)
               #'string<))))

    (cl-defmethod project-files ((project (head local)) &optional dirs)
      "Override `project-files' to use `fd' in local projects."
      (mapcan #'my/project-files-in-directory
              (or dirs (list (project-root project)))))))

(defun my/project-root (&optional maybe-prompt dir)
  "Return the project root in DIR,
MAYBE-PROMPT: if it is nil or omitted, return nil,
else ask the user for a directory in which to look for the project."
  (project-root (project-current maybe-prompt dir)))

(defalias '+project/root 'my/project-root)

(defun my/project-discover ()
  "Add dir under search-path to project."
  (interactive)
  (dolist (search-path '("~/code/" "~/git/"))
    (dolist (file (file-name-all-completions "" search-path))
      (when (not (member file '("./" "../")))
        (let ((full-name (expand-file-name file search-path)))
          (when (file-directory-p full-name)
            (when-let ((pr (project-current nil full-name)))
              (project-remember-project pr)
              (message "add project %s..." pr))))))))

(defun +project/project-buffer-filter (buffer)
  (let ((name (buffer-name buffer)))
    (or (and (string-prefix-p "*" name)
             (not (string-prefix-p "*eww*" name))
             (not (string-prefix-p "*ein: http" name))
             (not (string-prefix-p "*ein:notebooklist" name))
             (not (string-prefix-p "*vterm:" name))
             (not (string-prefix-p "*cider" name))
             (not (string-prefix-p "*Python" name)))
        (string-match-p "magit.*:" name)
        (equal (buffer-name (current-buffer)) name))))

(defun +project/project-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (buffer) (+project/project-buffer-filter buffer))
     buffers))

(defun my/project-buffers-a (fn project)
  (+project/project-buffer-filter-function
   (funcall fn project)))

(advice-add 'project-buffers :around 'my/project-buffers-a)

(defun my/project-switch-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (dired dir))


;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

;; TODO: workspace layout
;; https://www.youtube.com/watch?v=HRQhYAz3M-U

(use-package persp-mode
  :defer t)


(provide 'init-project)

;;; init-project.el ends here

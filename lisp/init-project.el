;; init-project.el --- project configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  project configurations
;;

;;; Code:

;; FIXME: `project-list-file' project.el remembered remote project
;; NOTE: https://emacstalk.github.io/post/010/ EmacsTalk
;; NOTE: https://github.com/karthink/project-x
;; `project-remember-project', `project-forget-project',
(use-package project
  :ensure nil
  :defer t
  :commands (project-root project-forget-zombie-projects project-prompt-project-dir)
  :config
  ;; auto clean up zombie projects
  (run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)

  (defun +project/project-buffer-filter (buffer)
    (let ((name (buffer-name buffer))
          (major-mode (with-current-buffer buffer major-mode)))
      (or (and (string-prefix-p "*" name)
               (not (string-prefix-p "*cider" name))
               (not (eq major-mode 'vterm-mode))
               (not (eq major-mode 'inferior-python-mode))
               (not (with-current-buffer buffer (bound-and-true-p gptel-mode))))
          (string-prefix-p " " name)
          (string-match-p "magit.*:" name))))

  (setq project-ignore-buffer-conditions '(+project/project-buffer-filter))

  ;; (defvar my/project-local-identifier '(".projectile" ".project" "go.mod" "Cargo.toml"
  ;;                                     "project.clj" "pom.xml" "package.json"
  ;;                                     "Makefile" "README.org" "README.md"))
  (defvar my/project-local-identifier '(".projectile" "pyproject.toml"))

  (defun my/project-try-local (dir)
    "Determine if DIR is a non-VC project."
    (if-let* ((root (if (listp my/project-local-identifier)
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
              (or dirs (list (project-root project)))))

    ;; (cl-defmethod project-files ((project (head vc)) &optional dirs)
    ;;   "Override `project-files' to use `fd' in local projects."
    ;;   (mapcan #'my/project-files-in-directory
    ;;           (or dirs (list (project-root project)))))
    ))

(defun my/project-root (&optional maybe-prompt dir)
  "Return the project root in DIR,
MAYBE-PROMPT: if it is nil or omitted, return nil,
else ask the user for a directory in which to look for the project."
  (project-root (project-current maybe-prompt dir)))

(defalias '+project/root 'my/project-root)

(defun my/project-switch-project (dir)
  (interactive (list (project-prompt-project-dir)))
  (dired dir))

;;;; Project Isolation

;; [envrc.el](https://github.com/purcell/envrc)
;; [direnv](https://direnv.net/docs/hook.html)
;; [nix-direnv](https://github.com/nix-community/nix-direnv)
;; `envrc-reload', `envrc-allow', `envrc-deny'
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;;; Compilation
;; `compilation-shell-minor-mode'
(use-package compile
  :defer t
  :init
  (require 'ansi-color)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-scroll-output t
        compilation-ask-about-save nil))


(provide 'init-project)

;;; init-project.el ends here

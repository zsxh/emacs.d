;; init-git.el --- Version Control Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Version Control Configuations
;;

;;; Code:

;; NOTE: https://magit.vc/manual/magit/Performance.html
(use-package magit
  :commands (magit magit-blame magit-file-popup)
  :defer 10
  :config
  (setq magit-bury-buffer-function #'magit-restore-window-configuration
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-format-file-function #'magit-format-file-nerd-icons
        magit-define-global-key-bindings nil
        magit-diff-refine-hunk 'all
        magit-save-repository-buffers 'dontask)

  ;; cache user password when using http, https://stackoverflow.com/a/75298815
  (add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)

  (defun magit-mode-bury-buffer-always-kill (&optional _)
    "Kill Magit buffers based on context.
When called from a `magit-status-mode' buffer, kills all related Magit buffers.
Otherwise, kill the current buffer using `magit-bury-buffer-function'."
    (interactive "P")
    (let ((current-mode major-mode))
      (funcall magit-bury-buffer-function t)
      (when (eq current-mode 'magit-status-mode)
        (mapc #'kill-buffer (magit-mode-get-buffers)))))

  (define-key magit-mode-map (kbd "q") #'magit-mode-bury-buffer-always-kill)

  ;; TIPS: keybindings "[" `magit-section-forward-sibling', "]" `magit-section-backward-sibling'
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit)
    (evil-collection-init 'magit-section)
    (evil-define-key '(normal visual) magit-mode-map
      "$" 'magit-process-buffer
      "q" 'magit-mode-bury-buffer-always-kill
      ;; "q" 'magit-mode-bury-buffer
      )
    (with-eval-after-load 'with-editor
      (evil-define-minor-mode-key 'normal 'with-editor-mode
        ",c" 'with-editor-finish
        ",k" 'with-editor-cancel))
    (with-eval-after-load 'magit-blame
      (evil-define-minor-mode-key 'normal 'magit-blame-mode
        "q" 'magit-blame-quit
        "c" 'magit-blame-cycle-style))))

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :after magit
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json" ".git/" ".venv/"))
  (magit-todos-ignored-keywords '("NOTE" "DONE" "HACK"))
  (magit-todos-auto-group-items 'always)
  :config
  (magit-todos-mode 1)
  ;; evil keybindings
  (with-eval-after-load 'evil-collection (evil-collection-init 'magit-todos))
  ;; consult
  (defun consult-magit-todos ()
    (interactive)
    (consult--read (magit-todos-candidates)
                   :prompt "TODOs: "
                   :sort nil
                   :history nil
                   :group (lambda (selected transform)
                            (if transform selected
                              (nth 0 (split-string selected))))
                   :lookup (lambda (selected candidates &rest _)
                             (magit-todos-jump-to-item
                              :item (consult--lookup-cdr selected candidates))))))

(use-package diff-hl
  :init
  (add-hook-run-once 'find-file-hook (lambda ()
                                       (global-diff-hl-mode)
                                       (unless (display-graphic-p)
                                         (diff-hl-margin-mode))))
  :defer t
  :config
  (setq diff-hl-disable-on-remote t
        diff-hl-update-async nil)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package forge
  :defer t
  :custom
  (forge-database-file (expand-file-name "cache/forge-database.sqlite" user-emacs-directory))
  :config
  ;; https://github.com/magit/forge/wiki/Tips-and-Tricks#accessing-private-gitlab-instances-via-http
  (defclass forge-gitlab-http-repository (forge-gitlab-repository)
    ((issues-url-format :initform "http://%h/%o/%n/issues")
     (issue-url-format :initform "http://%h/%o/%n/issues/%i")
     (issue-post-url-format :initform "http://%h/%o/%n/issues/%i#note_%I")
     (pullreqs-url-format :initform "http://%h/%o/%n/merge_requests")
     (pullreq-url-format :initform "http://%h/%o/%n/merge_requests/%i")
     (pullreq-post-url-format :initform "http://%h/%o/%n/merge_requests/%i#note_%I")
     (commit-url-format :initform "http://%h/%o/%n/commit/%r")
     (branch-url-format :initform "http://%h/%o/%n/commits/%r")
     (remote-url-format :initform "http://%h/%o/%n")
     (create-issue-url-format :initform "http://%h/%o/%n/issues/new")
     (create-pullreq-url-format :initform "http://%h/%o/%n/merge_requests/new")
     (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))
  (add-to-list 'ghub-insecure-hosts "git.private.network.repo/api/v4"))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "p" 'git-timemachine-show-previous-revision
    "n" 'git-timemachine-show-next-revision
    "q" 'git-timemachine-quit
    "b" 'git-timemachine-blame
    "gtg" 'git-timemachine-show-nth-revision
    "gtt" 'git-timemachine-show-revision-fuzzy
    "gty" 'git-timemachine-kill-abbreviated-revision
    "gtY" 'git-timemachine-kill-revision))

;; Show Version Control Software (VCS) commit message of current line.
;; https://github.com/redguardtoo/vc-msg
(use-package vc-msg
  :commands vc-msg-show
  :config
  (with-eval-after-load 'vc-msg-git
    ;; show code of commit
    (setq vc-msg-git-show-commit-function 'magit-show-commit)
    ;; open file of certain revision
    (push '("m"
            "[m]agit-find-file"
            (lambda ()
              (let* ((info vc-msg-previous-commit-info)
                     (git-dir (locate-dominating-file default-directory ".git")))
                (magit-find-file (plist-get info :id )
                                 (concat git-dir (plist-get info :filename))))))
          vc-msg-git-extra)))

;; This package provides several major modes for editing Git
;; configuration files.  The modes are:
;; `gitattributes-mode' for .gitattributes, .git/info/attributes, and git/attributes files;
;; `gitconfig-mode' for .gitignore, .git/info/exclude, and git/ignore files;
;; `gitignore-mode' for .gitignore, .git/info/exclude, and git/ignore files.
(use-package git-modes
  :defer t)

;; https://github.com/sshaw/git-link
(use-package git-link
  :defer t)

;; TODO: Emacs integration for Atlassian's Jira.
;; Supports listing and filtering issues, viewing issue details, modifying certain properties, and adding worklogs.
(use-package jira
  :defer t)

(use-package embark-vc
  :defer forge)


(provide 'init-git)

;;; init-git.el ends here

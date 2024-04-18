;; init-git.el --- Version Control Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Version Control Configuations
;;

;;; Code:

(use-package magit
  :commands (magit magit-blame magit-file-popup)
  :defer 10
  :config
  (setq magit-bury-buffer-function #'magit-restore-window-configuration
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-define-global-key-bindings nil
        magit-diff-refine-hunk 'all
        magit-save-repository-buffers 'dontask)
  ;; cache user password when using http, https://stackoverflow.com/a/75298815
  (add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit)
    (evil-collection-init 'magit-section)
    (evil-define-key '(normal visual) magit-mode-map
      "$" 'magit-process-buffer)
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
  :defer t
  ;; :init
  ;; (add-hook-run-once 'magit-mode-hook #'magit-todos-mode)
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json" ".git/" ".venv/"))
  ;; (magit-todos-update t)
  ;; magit-todos insert is slow for large repos, so toggle todos manually
  :commands (magit-todos-list consult-magit-todos)
  :config
  (setq magit-todos-auto-group-items 'always)
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit-todos))
  (with-eval-after-load 'evil
    (evil-collection-define-key nil 'magit-todos-item-section-map
      "j" nil))
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
  (unless (display-graphic-p)
    (add-hook 'after-init-hook #'diff-hl-margin-mode))
  :hook (after-init . global-diff-hl-mode)
  :config
  (setq diff-hl-disable-on-remote t)
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

(with-eval-after-load 'smerge-mode
  (require 'transient)
  (transient-define-prefix transient-smerge ()
    ["Smerge"
     ("j" "smerge-next" smerge-next :transient t)
     ("k" "smerge-prev" smerge-prev :transient t)
     ("0" "smerge-keep-current" smerge-keep-current :transient t)
     ("1" "smerge-keep-upper" smerge-keep-upper :transient t)
     ("2" "smerge-keep-base" smerge-keep-base :transient t)
     ("3" "smerge-keep-lower" smerge-keep-lower :transient t)
     ("4" "smerge-keep-all" smerge-keep-all :transient t)
     ("q" "quit" transient-quit-all)]))


(provide 'init-git)

;;; init-git.el ends here

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
  (setq magit-bury-buffer-function 'magit-mode-quit-window))

;; FIXME: Performance Issue caused by overlays, https://github.com/dandavison/magit-delta/issues/9
(use-package magit-delta
  ;; :hook (magit-mode . magit-delta-mode)
  :defer t)

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :defer t
  ;; :hook (magit-mode . magit-todos-mode)
  ;; :after magit
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json" ".git/"))
  ;; (magit-todos-update t)
  ;; magit-todos insert is slow for large repos, so toggle todos manually
  :commands (magit-todos-list)
  :config
  (magit-todos-mode)
  (setq magit-todos-auto-group-items 'always)
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit-todos))
  (with-eval-after-load 'evil
    (evil-collection-define-key nil 'magit-todos-item-section-map
      "j" nil)))

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
     ("1" "smerge-keep-upper" smerge-keep-upper :transient t)
     ("2" "smerge-keep-lower" smerge-keep-lower :transient t)
     ("q" "quit" transient-quit-all)]))


(provide 'init-git)

;;; init-git.el ends here

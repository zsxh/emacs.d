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
  (setq magit-bury-buffer-function 'magit-mode-quit-window)
  (require 'magit-todos))

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  ;; :hook (magit-mode . magit-todos-mode)
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

;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :after magit
  :config
  ;; fringe
  (let* ((height (* 2 (frame-char-height)))
         (width 2)
         (ones (1- (expt 2 width)))
         (bits (make-vector height ones)))
    (define-fringe-bitmap '+git/diff-hl-bitmap bits height 2))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) '+git/diff-hl-bitmap))
  ;; enable diff-hl
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (with-eval-after-load 'autorevert
    ;; NOTE: `auto-revert-handler' call `vc-refresh-state' when `auto-revert-check-vc-info' is t,
    ;; it brokes vc state that diff-hl rely on.
    ;; https://github.com/magit/magit/issues/4073#issuecomment-602072585
    (when auto-revert-check-vc-info
      (advice-add 'diff-hl-magit-pre-refresh :before (lambda () (setq auto-revert-check-vc-info nil)))
      (advice-add 'diff-hl-magit-post-refresh :after (lambda () (setq auto-revert-check-vc-info t))))))

;; This package provides several major modes for editing Git
;; configuration files.  The modes are:
;; `gitattributes-mode' for .gitattributes, .git/info/attributes, and git/attributes files;
;; `gitconfig-mode' for .gitignore, .git/info/exclude, and git/ignore files;
;; `gitignore-mode' for .gitignore, .git/info/exclude, and git/ignore files.
(use-package git-modes
  :defer t)

;; TODO: https://github.com/Artawower/blamer.el
(use-package blamer
  :defer t
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

;; hydra `smerge-next', `smerge-keep-upper', `smerge-keep-lower'
(with-eval-after-load 'smerge-mode
  (defhydra hydra-smerge (:hint nil)
    "
    ^Smerge^
    ^^^^^----------------------
    [_j_]: smerge-next
    [_k_]: smerge-prev
    [_1_]: smerge-keep-upper
    [_2_]: smerge-keep-lower
    "
    ("j" smerge-next)
    ("k" smerge-prev)
    ("1" smerge-keep-upper)
    ("2" smerge-keep-lower)
    ("q" nil "quit")))


(provide 'init-git)

;;; init-git.el ends here

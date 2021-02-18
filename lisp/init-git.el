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
  ;; https://github.com/magit/magit/issues/2371#issuecomment-152746346
  ;; value nil, vc mode-line update when buffer changed. t, update every auto-revert-interval seconds
  ;; (setq auto-revert-check-vc-info t)
  (setq magit-bury-buffer-function 'magit-mode-quit-window))

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  ;; :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-exclude-globs '("node_modules" "*.json"))
  ;; magit-todos insert is slow for large repos, so toggle todos manually
  :commands (magit-todos-list)
  :config
  (setq magit-todos-auto-group-items 'always)
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit-todos))
  (with-eval-after-load 'evil
    (evil-collection-define-key nil 'magit-todos-item-section-map
      "j" nil)))

;; FIXME: gitlab instances via http
;; https://github.com/magit/forge/issues/9
(use-package forge
  :defer t)

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

;; https://github.com/dandavison/magit-delta
;; poor performance when diffing too many lines
(use-package magit-delta
  :defer t
  ;; :if (lambda nil (executable-find "delta"))
  ;; :after magit
  :config
  (setq magit-delta-hide-plus-minus-markers nil)
  (magit-delta-mode))

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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(provide 'init-git)

;;; init-git.el ends here

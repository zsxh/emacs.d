;; zsxh-theme-custom.el --- Summary	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Commentary
;;

;;; Code:

;; TODO: 使用 deftheme 来修改样式
;; https://emacstalk.github.io/post/008/

(defun zsxh-fix-theme ()
  "Load zsxh customize theme faces."
  (pcase current-theme
    ('doom-nord-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#3B6EA8"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))))
    ('doom-one-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#3B6EA8"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))))
    ('doom-one
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#51afef"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))))
    ('doom-solarized-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#268bd2"))
       (with-eval-after-load 'paren
         (set-face-background 'show-paren-match "#E5E5E5"))))
    ('doom-dark+
     (progn
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#C586C0"))
       (set-face-background 'fringe (face-attribute 'default :background))
       (with-eval-after-load 'company-posframe
         (set-face-background 'company-posframe-active-backend-name (doom-color 'modeline-bg))
         (set-face-background 'company-posframe-inactive-backend-name (doom-color 'modeline-bg-alt)))
       (with-eval-after-load 'paren
         (set-face-background 'show-paren-match "#4e4e4e")))))

  ;; global settings
  (with-eval-after-load 'magit-diff
    (set-face-attribute 'magit-diff-revision-summary nil :inherit 'magit-diff-hunk-heading-highlight)))


(provide 'zsxh-theme-custom)

;;; zsxh-theme-custom.el ends here

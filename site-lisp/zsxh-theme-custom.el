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
  (with-eval-after-load 'font-lock
    (set-face-italic 'font-lock-keyword-face t)
    (let* ((func-name-fg (face-foreground font-lock-function-name-face))
           (theme-type (frame-parameter nil 'background-mode))
           (bg (if (string-equal "light" theme-type)
                   (doom-lighten func-name-fg 0.93)
                 (doom-darken func-name-fg 0.75))))
      (set-face-background 'font-lock-function-name-face bg)))

  (pcase current-theme
    ('doom-nord-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#3B6EA8"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))
       (with-eval-after-load 'markdown-mode
         (set-face-background 'markdown-code-face "#E0E0E0"))
       (with-eval-after-load 'org
         ;; Org block face
         (set-face-background 'org-block "#E0E0E0")
         (set-face-background 'org-quote nil)
         (set-face-background 'org-block-begin-line nil)
         (set-face-background 'org-block-end-line nil))
       (with-eval-after-load 'mmm-vars
         (set-face-background 'mmm-default-submode-face "#E5E5E5"))
       (with-eval-after-load 'jupyter-repl
         (set-face-foreground 'jupyter-repl-input-prompt "#4F894C")
         (set-face-background 'jupyter-repl-traceback "#FBF8EF"))))
    ('doom-one-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#3B6EA8"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))
       (with-eval-after-load 'lsp-headerline
         (set-face-foreground 'lsp-headerline-breadcrumb-separator-face (doom-color 'fg)))))
    ('doom-one
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#51afef"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))
       (with-eval-after-load 'org
         (set-face-background 'org-quote nil)
         (set-face-background 'org-block-begin-line nil)
         (set-face-background 'org-block-end-line nil)
         (set-face-attribute 'org-verbatim nil
                             :foreground "#98be65"
                             :background "#2e332d"
                             :inherit 'fixed-pitch))
       (with-eval-after-load 'ein-cell
         (set-face-attribute 'ein:cell-input-area nil :background "#22262e")
         (set-face-attribute 'ein:cell-input-prompt nil :foreground "#4F894C" :background "#282c34")
         (set-face-attribute 'ein:cell-output-prompt nil :foreground "darkred" :background "#282c34"))
       (with-eval-after-load 'jupyter-repl
         (set-face-foreground 'jupyter-repl-input-prompt "#4F894C")
         (set-face-background 'jupyter-repl-traceback "#4B483F"))
       (with-eval-after-load 'ivy
         (let ((base3 (doom-color 'base3))
               (blue (doom-color 'blue))
               (magenta (doom-color 'magenta))
               (green (doom-color 'green))
               (yellow (doom-color 'yellow))
               (violet (doom-color 'violet)))
           (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground yellow :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground magenta :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground green :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground blue :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-highlight nil :foreground violet :weight 'bold)
           (with-eval-after-load 'swiper
             (set-face-attribute 'swiper-match-face-1 nil :inherit 'ivy-minibuffer-match-face-1)
             (set-face-attribute 'swiper-match-face-2 nil :inherit 'ivy-minibuffer-match-face-2)
             (set-face-attribute 'swiper-match-face-3 nil :inherit 'ivy-minibuffer-match-face-3)
             (set-face-attribute 'swiper-match-face-4 nil :inherit 'ivy-minibuffer-match-face-4))))
       (with-eval-after-load 'lsp-headerline
         (set-face-foreground 'lsp-headerline-breadcrumb-separator-face (doom-color 'fg)))))
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

  ;; https://www.reddit.com/r/emacs/comments/diahh1/emacs_27_update_changed_how_highlighted_lines/
  ;; The new face attribute ':extend' controls whether to use the face for displaying the empty space beyond end of line (EOL) till the edge of the window.
  (when (fboundp 'set-face-extend)
    (with-eval-after-load 'org
      (set-face-extend 'org-block t)
      (set-face-extend 'org-block-begin-line t)
      (set-face-extend 'org-block-end-line t))
    (with-eval-after-load 'ein-cell
      (set-face-extend 'ein:cell-input-area t))
    (with-eval-after-load 'jupyter-repl
      (set-face-extend 'jupyter-repl-traceback t)))

  ;; global settings
  (with-eval-after-load 'magit-diff
    (set-face-attribute 'magit-diff-revision-summary nil :inherit 'magit-diff-hunk-heading-highlight))

  (with-eval-after-load 'diff-hl
    (let ((bg (face-attribute 'default :background)))
      (set-face-background 'diff-hl-insert bg)
      (set-face-background 'diff-hl-delete bg)
      (set-face-background 'diff-hl-change bg))))


(provide 'zsxh-theme-custom)

;;; zsxh-theme-custom.el ends here

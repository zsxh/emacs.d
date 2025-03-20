;; init-dired.el --- Dired Extentions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Dired Extentions
;;

;;; Code:

;; Lazy load `dirvish' to speedup bootstrap time
(defvar dirvish-override-dired-p nil)

(defun dirvish-override-dired-mode-maybe (&rest _)
  "Call `dirvish-override-dired-mode' only once."
  (unless dirvish-override-dired-p
    (setq dirvish-override-dired-p t)
    (dirvish-override-dired-mode)
    (dirvish-peek-mode)))

(advice-run-once 'find-file :before #'dirvish-override-dired-mode-maybe)

;; https://github.com/alexluigit/dirvish
(use-package dirvish
  :defer 10
  :bind (:map dired-mode-map
         ("C-<return>" . 'dired-do-open)
         ("TAB" . 'dirvish-subtree-toggle)
         ("f" . dirvish-file-info-menu)
         ("h" . dired-omit-mode)
         ("K" . dired-up-directory)
         ("l" . nil)
         ("N" . dirvish-narrow)
         ;; "o" `dired-find-file-other-window'
         ("M-a" . dirvish-mark-actions-menu)
         ("M-m" . dirvish-setup-menu)
         ("M-f" . dirvish-layout-toggle)
         ("M-h" . dirvish-show-history)
         ([remap dired-summary] . dirvish-dispatch) ; "?"
         ([remap dired-sort-toggle-or-edit] . dirvish-ls-switches-menu) ; "s"
         ([remap dired-do-copy] . dirvish-yank) ; "C", "P" copy
         ;; ("R". dired-do-rename) ; "R" rename
         ("M" . dirvish-move) ; "M" move
         ([remap mode-line-other-buffer] . dirvish-other-buffer)
         ("." . dired-omit-mode) ;; toggle dotfiles
         )
  :init
  (defvar server-buffer-clients '())
  :config
  (require 'dirvish-vc nil t)
  (require 'dirvish-emerge nil t)
  (advice-run-once
   'dirvish-pdf-dp
   :before (lambda (&rest _)
             (ignore-errors
               (require 'pdf-tools nil t))))

  (setq dirvish-time-format-string "%F %R"
        dirvish-attributes
        (append
         ;; The order of these attributes is insignificant, they are always
         ;; displayed in the same position.
         '(vc-state subtree-state nerd-icons)
         ;; Other attributes are displayed in the order they appear in this list.
         '(git-msg file-modes file-time file-size))
        dirvish-mode-line-format '(:left (bar winum sort file-time symlink) :right (omit yank vc-info index))
        dirvish-mode-line-height (or (bound-and-true-p doom-modeline-height) (+ (frame-char-height) 4))
        dirvish-cache-dir (expand-file-name (locate-user-emacs-file "cache/dirvish/"))
        dirvish-reuse-session 'open
        dirvish-redisplay-debounce 0.05 ;; perf: increase redisplay intervals
        dirvish-emerge-groups '(("Recent files" (predicate . recent-files-2h))
                                ("Directories" (predicate . directories))
                                ("Documents" (extensions "pdf" "tex" "bib" "epub"))
                                ("Video" (extensions "mp4" "mkv" "webm"))
                                ("Pictures" (extensions "jpg" "png" "svg" "gif"))
                                ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                                ("Archives" (extensions "gz" "rar" "zip"))
                                ("Files" (predicate . files))))
  (setopt dirvish-subtree-state-style 'nerd)
  ;; (add-hook 'dirvish-setup-hook #'dirvish-emerge-mode)

  (dirvish-override-dired-mode-maybe)

  (with-eval-after-load 'evil
    (evil-define-key 'normal dirvish-mode-map
      "q" 'dirvish-quit))

  (dirvish-define-mode-line bar "doom-modeline bar"
    (when (bound-and-true-p doom-modeline-mode)
      (doom-modeline--bar)))
  (advice-add 'dirvish--mode-line-bar-img :override #'ignore)

  (dirvish-define-mode-line winum "`winum' indicator"
    (setq winum-auto-setup-mode-line nil)
    (if-let* ((_ (bound-and-true-p winum-mode))
              (_ (bound-and-true-p doom-modeline-mode))
              (num (winum-get-number-string))
              (_ (not (string-equal "0" num)))
              (_ (and (< 0 (length num))
                      (< 1 (length (cl-mapcan
                                    (lambda (frame)
                                      ;; Exclude minibuffer and child frames
                                      (unless (and (fboundp 'frame-parent)
                                                   (frame-parent frame))
                                        (window-list frame 'never)))
                                    (visible-frame-list)))))))
        (propertize (format " %s " num)
                    'face (if (doom-modeline--active)
                              'doom-modeline-buffer-major-mode
                            'mode-line-inactive))
      " "))

  (when (executable-find "pdftoppm")
    ;; Dirvish provided an alternative PDF preview dispatcher pdf-preface which
    ;; generates preface image for pdf files and use those preface images as the preview.
    ;; This allows the user to preview big pdf files in a non-blocking fashion.
    (setq dirvish-preview-dispatchers
          (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)))

  (when (executable-find "eza")
    (dirvish-define-preview eza (file)
      "Use `eza' to generate directory preview."
      :require ("eza") ; tell Dirvish to check if we have the executable
      (when (file-directory-p file) ; we only interest in directories here
        `(shell . ("eza" "-al" "--color=always" "--icons"
                   "--group-directories-first" ,file))))
    (push 'eza dirvish-preview-dispatchers)))

(use-package dirvish-side
  :ensure dirvish
  :commands (+dirvish/project-root-side)
  :config
  (setq dirvish-side-width 25)

  (defun +dirvish/project-root-side ()
    (interactive)
    (if-let* ((dirvish-side-win (dirvish-side--session-visible-p)))
        (with-selected-window dirvish-side-win
          (dirvish-quit))
      (dirvish-side--new (or (project-root (project-current))
                             default-directory))))

  (with-eval-after-load 'winum
    (defun +dirvish/winum-assign-func ()
      (when (and (functionp 'dirvish-side--session-visible-p)
                 (eq (selected-window) (dirvish-side--session-visible-p))
                 (eq (selected-window) (frame-first-window)))
        0))
    (add-to-list 'winum-assign-functions #'+dirvish/winum-assign-func))

  (with-eval-after-load 'ace-window
    (define-advice aw-ignored-p (:around (orig-fn window) dirvish-advice)
      (or (funcall orig-fn window)
          (and (functionp 'dirvish-side--session-visible-p)
               (eq window (dirvish-side--session-visible-p)))))))

(use-package dired
  :ensure nil
  :defer t
  :config
  (dirvish-override-dired-mode-maybe)

  ;; NOTE: install nixpkgs `coreutils'
  (setq dired-listing-switches "-alhA --time-style=long-iso --group-directories-first --no-group")

  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; dired-kill-when-opening-new-dired-buffer t
        ;; dired "human-readable" format
        dired-mouse-drag-files t
        dired-auto-revert-buffer #'dired-directory-changed-p)

  (defun +dried/dired-do-delete-a (fn &rest args)
    (let ((delete-by-moving-to-trash (and (not (file-remote-p default-directory))
                                          delete-by-moving-to-trash)))
      (apply fn args)))

  (advice-add 'dired-do-delete :around '+dried/dired-do-delete-a)

  (defun +dired/get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
      (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message
         "Size of all marked files: %s"
         (progn
           (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
           (match-string 1))))))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "SPC") nil
      "," nil
      ":" 'evil-ex
      "a" 'dired-create-empty-file
      "d" 'dired-flag-file-deletion
      "F" 'dired-create-empty-file
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line
      "i" 'dired-toggle-read-only
      "j" 'dired-next-line
      "k" 'dired-previous-line
      "q" 'quit-window
      "v" 'evil-visual-char
      "V" 'evil-visual-line))

  (+funcs/major-mode-leader-keys
   dired-mode-map
   "N" '(dirvish-narrow :which-key "dirvish-narrow")
   "M" '(dirvish-move :which-key "move-file")
   "P" '(dirvish-yank :which-key "paste-file")
   "s" '(+dired/get-size :which-key "get-size")
   "." '(dired-omit-mode :which-key "toggle-dotfiles")))

(use-package dired-x
  :ensure nil
  :defer t
  ;; Enable dired-omit-mode by default
  ;; :hook
  ;; (dired-mode . dired-omit-mode)
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Editable Dired pre-mode configs
(use-package wdired
  :ensure nil
  :defer t
  :config
  ;; allow editing file permissions
  (setq wdired-allow-to-change-permissions t)

  (+funcs/major-mode-leader-keys
   wdired-mode-map
   "N" '(consult-focus-lines :which-key "consult-focus-lines")
   "c" '(wdired-finish-edit :which-key "finish edit")
   "k" '(wdired-abort-changes :which-key "abort changes")
   "q" '(wdired-exit :which-key "exit")))


(provide 'init-dired)

;;; init-dired.el ends here

;; init-dired.el --- Dired Extentions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Dired Extentions
;;

;;; Code:

;; TODO: replace neotree with dirvish-side

;; Lazy load `dirvish' to speedup bootstrap time
(defvar dirvish-override-dired-p nil)

(defun dirvish-override-dired-mode-maybe (&rest _)
  "Call `dirvish-override-dired-mode' only once."
  (unless dirvish-override-dired-p
    ;; (message "debug: load dirvish")
    (setq dirvish-override-dired-p t)
    (dirvish-override-dired-mode)))

(advice-run-once 'find-file :before #'dirvish-override-dired-mode-maybe)

;; https://github.com/alexluigit/dirvish/blob/main/Configuration.org
(use-package dirvish
  :defer t
  :custom
  ;; Feel free to replace `all-the-icons' with `vscode-icon'.
  (dirvish-time-format-string "%F %R")
  (dirvish-attributes '(subtree-state all-the-icons file-size))
  (dirvish-mode-line-format '(:left (bar winum sort omit file-time) :right (vc-info index)))
  (dirvish-keep-alive-on-quit t)
  :bind (:map dired-mode-map
              ("C-<return>" . 'dired-open-xdg)
              ("TAB" . 'dirvish-toggle-subtree)
              ("f" . dirvish-file-info-menu)
              ("h" . dired-omit-mode)
              ("K" . dired-up-directory)
              ("l" . nil)
              ("N" . consult-focus-lines)
              ("M-a" . dirvish-mark-actions-menu)
              ("M-c" . dired-collapse-mode)
              ("M-m" . dirvish-setup-menu)
              ("M-f" . dirvish-toggle-fullscreen)
              ("M-h" . dirvish-show-history)
              ([remap dired-summary] . dirvish-dispatch) ; "?"
              ([remap dired-sort-toggle-or-edit] . dirvish-ls-switches-menu) ; "s"
              ([remap dired-do-copy] . dirvish-yank) ; "C" copy
              ;; ("R". dired-do-rename) ; "R" rename
              ("M" . dirvish-move)      ; "M" move
              ([remap mode-line-other-buffer] . dirvish-other-buffer)
              ("." . dired-omit-mode) ;; toggle dotfiles
              )
  :config
  (dirvish-override-dired-mode-maybe)
  ;; (dirvish-override-dired-mode)
  ;; (dirvish-peek-mode)

  (dirvish-define-mode-line bar "doom-modeline bar"
    (when (bound-and-true-p doom-modeline-mode)
      (doom-modeline--bar)))

  (dirvish-define-mode-line winum "`winum' indicator"
    (setq winum-auto-setup-mode-line nil)
    (if-let* ((_ (bound-and-true-p winum-mode))
              (_ (bound-and-true-p doom-modeline-mode))
              (num (winum-get-number-string))
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
      (doom-modeline-spc)))

  (when (executable-find "exa")
    (dirvish-define-preview exa (file)
      "Use `exa' to generate directory preview."
      (when (file-directory-p file) ; we only interest in directories here
        `(shell . ("exa" "--color=always" "-al" ,file)))) ; use the output of `exa' command as preview
    (add-to-list 'dirvish-preview-dispatchers 'exa)))

(use-package dired
  :ensure nil
  :defer t
  :config
  (dirvish-override-dired-mode-maybe)

  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; dired-kill-when-opening-new-dired-buffer t
        ;; dired "human-readable" format
        dired-listing-switches "-alh --time-style=long-iso --group-directories-first --no-group"
        dired-mouse-drag-files t)

  (defun +dried/dired-do-delete-a (fn &rest args)
    (let ((delete-by-moving-to-trash (and (not (file-remote-p default-directory))
                                          delete-by-moving-to-trash)))
      (apply fn args)))

  (advice-add 'dired-do-delete :around '+dried/dired-do-delete-a)

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
   "N" '(consult-focus-lines :which-key "consult-focus-lines")
   "M" '(lambda () (interactive) (dirvish-yank 'move) :which-key "move-file")
   "P" '(dirvish-yank :which-key "paste-file")
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

;; Addtional syntax highlighting for dired
;; (use-package diredfl
;;   :hook
;;   (dired-mode . diredfl-mode))

(use-package dired-collapse
  :commands dired-collapse-mode)

;; open files with external applications(just for linux now)
(use-package dired-open
  :commands dired-open-xdg)

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


(defalias '+dired/find-program 'find-name-dired)
(with-eval-after-load 'find-dired
  (setq find-ls-option
        (cons "-print0 | xargs -0 ls -alhdN" "")))

;; Drop-in replacement for find-dired
;; install rust fd first: pacman -S fd
;; https://github.com/sharkdp/fd
;; dired find files using 'fd' instead of 'find'
;; https://github.com/yqrashawn/fd-dired
(use-package fd-dired
  :if (executable-find "fd")
  :init
  (defalias '+dired/find-program 'fd-dired)
  :commands fd-dired
  :config
  (setq fd-dired-pre-fd-args "-0 -c never -I"
        fd-dired-ls-option '("| xargs -0 ls -alhdN" . "-ld")))


(provide 'init-dired)

;;; init-dired.el ends here

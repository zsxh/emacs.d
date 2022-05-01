;; init-dired.el --- Dired Extentions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Dired Extentions
;;

;;; Code:

;; TODO: replace neotree with dirvish-side

;; https://github.com/alexluigit/dirvish/blob/main/Configuration.org
(use-package dirvish
  :defer t
  :custom
  ;; Feel free to replace `all-the-icons' with `vscode-icon'.
  (dirvish-attributes '(expanded-state all-the-icons file-size))
  :bind (:map dired-mode-map
              ("C-<return>" . 'dired-open-xdg)
              ("TAB" . 'dired-subtree-toggle)
              ("f" . dirvish-file-info-menu)
              ("K" . dired-up-directory)
              ("N" . dired-narrow)
              ("M-a" . dirvish-mark-actions-menu)
              ("M-m" . dirvish-setup-menu)
              ("M-f" . dirvish-toggle-fullscreen)
              ("M-h" . dirvish-show-history)
              ([remap dired-summary] . dirvish-dispatch)        ; "?"
              ([remap dired-sort-toggle-or-edit] . dirvish-ls-switches-menu) ; "s"
              ([remap dired-do-copy] . dirvish-yank) ; "C" copy, "C-u C" move, "R" rename
              ([remap mode-line-other-buffer] . dirvish-other-buffer)
              ([remap dired-omit-mode] . dired-filter-mode))
  :config
  (dirvish-override-dired-mode)
  ;; (dirvish-peek-mode)
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
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        ;; dired-kill-when-opening-new-dired-buffer t
        ;; dired "human-readable" format
        dired-listing-switches "-alh --time-style=long-iso --group-directories-first --no-group")

  (require 'dirvish)

  (defun +dried/dired-do-delete-a (fn &rest args)
    (let ((delete-by-moving-to-trash (and (not (file-remote-p default-directory))
                                          delete-by-moving-to-trash)))
      (apply fn args)))

  (advice-add 'dired-do-delete :around '+dried/dired-do-delete-a)

  (with-eval-after-load 'evil-collection
    (evil-collection-init 'dired)
    (evil-define-key 'normal dired-mode-map
      (kbd "SPC") nil
      "," nil
      ":" 'evil-ex
      "F" 'dired-create-empty-file
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line
      "h" 'evil-backward-char
      "K" 'dired-up-directory
      "l" 'evil-forward-char
      "N" 'dired-narrow
      "q" 'quit-window
      "s" 'dirvish-ls-switches-menu
      "v" 'evil-visual-char
      "V" 'evil-visual-line))

  (+funcs/major-mode-leader-keys
   dired-mode-map
   "/" '(dired-narrow :which-key "dired-narrow")
   "?" '(dirvish-dispatch :which-key "dirvish-dispatch")
   "a" '(dirvish-mark-actions-menu :which-key "mark-actions-menu")
   "f" '(dirvish-file-info-menu :which-key "file-info-menu")
   "F" '(dirvish-toggle-fullscreen :which-key "dirvish-toggle-fullscreen")
   "h" '(dirvish-show-history :which-key "dirvish-show-history")
   "m" '(dirvish-setup-menu :which-key "dirvish-setup-menu")
   "M" '(lambda () (interactive) (dirvish-yank 'move) :which-key "move-file")
   "P" '(dirvish-yank :which-key "paste-file")
   "r" '(dired-narrow-regexp :which-key "dired-narrow-regexp")
   "s" '(dirvish-ls-switches-menu :which-key "dirvish-ls-switches-menu")
   "T" '(dired-filter-mode :which-key "toggle-dired-filter-mode")))

(use-package dired-x
  :ensure nil
  ;; Enable dired-omit-mode by default
  ;; :hook
  ;; (dired-mode . dired-omit-mode)
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

;; Turn Dired into a tree browser
(use-package dired-subtree
  :commands (dired-subtree-toggle)
  :config
  (setq dired-subtree-use-backgrounds nil))

;; Narrow a dired buffer to the files matching a string.
(use-package dired-narrow
  :commands (dired-narrow))

;; `ibuffer' like file filtering system
(use-package dired-filter
  :after dirvish
  :config
  :custom
  ;; Do not touch the header line
  (dired-filter-show-filters nil)
  (dired-filter-revert 'always))

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
   "/" '(dired-narrow :which-key "dired-narrow")
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

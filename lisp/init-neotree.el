;; init-neotree.el --- Neotree	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Neotree
;;

;;; Code:

(use-package neotree
  :init
  (setq neo-show-hidden-files t)
  :commands neo-global--window-exists-p
  :config
  (advice-add 'neo-open-file
              :before (lambda (full-path &optional arg)
                        (unless (neo-window--minimize-p)
                          (neotree-stretch-toggle))))

  (with-eval-after-load 'evil-collection
    ;; Evil-Keybindings
    (evil-collection-init 'neotree)
    ;; Custom Keybindings
    (evil-define-key 'normal neotree-mode-map
      "gd" '+neotree/jump-to-dired
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line
      "h" '+neotree/neotree-collapse-or-up
      "l" '+neotree/neotree-expand
      "K" 'neotree-select-up-node
      "J" 'neotree-select-down-node
      "R" 'neotree-change-root
      "\C-a" 'move-beginning-of-line
      "\C-e" 'move-end-of-line))

  (with-eval-after-load 'winum
    ;; window 0 is reserved for file trees
    (add-to-list 'winum-assign-functions #'+neotree/winum-neotree-assign-func)))

(defun +neotree/jump-to-dired (&optional arg)
  "Jump to Dired buffer corresponding to current neotree node."
  (interactive)
  (unless (neo-window--minimize-p)
    (neotree-stretch-toggle))
  (let ((dir default-directory))
    (neo-global--select-mru-window arg)
    (find-file dir)))

;; Code from Spacemacs
(defun +neotree/neotree-expand-or-open (&optional arg)
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (if arg
            (neotree-enter arg)
          (let ((mru-winum (winum-get-number (get-mru-window))))
            (apply 'neotree-enter (list mru-winum))))))))

(defun +neotree/neotree-expand ()
  "Expand a neotree node"
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))))))

(defun +neotree/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun +neotree/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (+neotree/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun +neotree/winum-neotree-assign-func ()
  "Custom number assignment for neotree."
  (when (and (boundp 'neo-buffer-name)
             (string= (buffer-name) neo-buffer-name)
             ;; in case there are two neotree windows. Example: when
             ;; invoking a transient state from neotree window, the new
             ;; window will show neotree briefly before displaying the TS,
             ;; causing an error message. the error is eliminated by
             ;; assigning 0 only to the top-left window
             (eq (selected-window) (frame-first-window)))
    0))

(defun +neotree/find-project-root ()
  "Select project root as node in NeoTree."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

;; FIXME: electric-indent-local-mode arg issue
(with-eval-after-load 'neotree
  (define-derived-mode neotree-mode special-mode "NeoTree"
    "A major mode for displaying the directory tree in text mode."
    (setq indent-tabs-mode nil          ; only spaces
          buffer-read-only t            ; read only
          truncate-lines -1
          neo-buffer--show-hidden-file-p neo-show-hidden-files)
    (when neo-hide-cursor
      (progn
        (setq cursor-type nil)
        (hl-line-mode +1)))
    (pcase neo-mode-line-type
      (`neotree
       (setq-local mode-line-format neo-mode-line-format)
       (add-hook 'post-command-hook 'force-mode-line-update nil t))
      (`none (setq-local mode-line-format nil))
      (`custom
       (setq-local mode-line-format neo-mode-line-custom-format)
       (add-hook 'post-command-hook 'force-mode-line-update nil t))
      (_ nil))
    ;; fix for electric-indent-mode
    ;; for emacs 24.4
    ;; (if (fboundp 'electric-indent-local-mode)
    ;;     (electric-indent-local-mode -1)
    ;;   ;; for emacs 24.3 or less
    ;;   (add-hook 'electric-indent-functions
    ;;             (lambda (arg) 'no-indent) nil 'local))
    (when neo-auto-indent-point
      (add-hook 'post-command-hook 'neo-hook--node-first-letter nil t))))


(provide 'init-neotree)

;;; init-neotree.el ends here

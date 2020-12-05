;; init-neotree.el --- Neotree	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Neotree
;;

;;; Code:

;; TODO: Combine directories with only one child to make display more compact
;; https://github.com/jaypei/emacs-neotree/issues/147
;; https://github.com/jaypei/emacs-neotree/issues/85
;; need to modify neo-buffer--insert-tree and neo-buffer--insert-dir-entry.
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
      (neotree-find (+project/root))
      (neotree-find origin-buffer-file-name))))

;; HACK neotree
;; https://github.com/jaypei/emacs-neotree/issues/147
;; directory paths collapsed.
(defun neo-get-collapse-node (node)
  (condition-case nil
      (let ((entries (cl-remove-if
                      (lambda (file)
                        (or
                         (equal (file-name-nondirectory (directory-file-name file)) ".")
                         (equal (file-name-nondirectory (directory-file-name file)) "..")))
                      (directory-files node t))))
        (if (and (= (length entries) 1)
                 (file-directory-p (car entries)))
            (neo-get-collapse-node (car entries))
          node))
    ('file-error
     (message "Walk directory %S failed." node)
     node)))

(defun neo-buffer--insert-tree-a (path depth)
  (if (eq depth 1)
      (neo-buffer--insert-root-entry path))
  (let* ((contents (neo-buffer--get-nodes path))
         (nodes (car contents))
         (leafs (cdr contents))
         (default-directory path))
    (dolist (node nodes)
      (let* ((collapse-node (neo-get-collapse-node node))
             (expanded (neo-buffer--expanded-node-p collapse-node))
             (collapsed (> (length collapse-node) (length node)))
             (short-name (if collapsed
                             (substring collapse-node (1+ (length (neo-path--updir node))))
                           nil))
             (node (if collapsed collapse-node node)))
        (neo-buffer--insert-dir-entry
         node depth expanded short-name)
        (if expanded (neo-buffer--insert-tree (concat node "/") (+ depth 1)))))
    (dolist (leaf leafs)
      (neo-buffer--insert-file-entry leaf depth))))

(defun neo-buffer--insert-dir-entry-a (node depth expanded &optional short-name)
  (let ((node-short-name (or short-name (neo-path--file-short-name node))))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char ?\s 2))
    (neo-buffer--insert-fold-symbol
     (if expanded 'open 'close) node)
    (insert-button (if neo-show-slash-for-folder (concat node-short-name "/") node-short-name)
                   'follow-link t
                   'face neo-dir-link-face
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap
                   'help-echo (neo-buffer--help-echo-message node-short-name))
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(advice-add #'neo-buffer--insert-tree :override #'neo-buffer--insert-tree-a)
(advice-add #'neo-buffer--insert-dir-entry :override #'neo-buffer--insert-dir-entry-a)

(with-eval-after-load '(and neotree doom-themes-ext-neotree)

  (defun doom-themes-neotree-insert-dir-a (node depth expanded &optional short-name)
    (let ((short-name (or short-name (neo-path--file-short-name node)))
          (faces '(doom-themes-neotree-dir-face))
          icon-text)
      ;; insert indentation
      (insert-char ?\s (* (- depth 1) 2))
      ;; vcs integration
      (let ((vc (if neo-vc-integration (neo-vc-for-node node))))
        (when (memq 'char neo-vc-integration)
          (insert-char (car vc))
          (insert-char ?\s))
        (unless (and (memq 'face neo-vc-integration)
                     (not (eq (cdr vc) 'neo-vc-up-to-date-face))
                     (setq faces (list (cdr vc))))
          (cl-destructuring-bind (&key face icon)
              (doom-themes-neotree-spec node doom-themes-neotree-dir-rules)
            (if face (push face faces))
            (if icon (setq icon-text icon)))))
      ;; insert icon
      (let ((type (if expanded 'open 'close)))
        (if (display-graphic-p)
            (doom--neotree-insert-icon type node icon-text faces)
          (neo-buffer--insert-fold-symbol type node)))
      ;; insert label button
      (when doom-themes-neotree-enable-variable-pitch
        (push 'variable-pitch faces))
      (insert-button short-name
                     'follow-link t
                     'face `(:inherit (,@faces))
                     'neo-full-path node
                     'keymap neotree-dir-button-keymap)
      ;; metadata + newline
      (neo-buffer--node-list-set nil node)
      (neo-buffer--newline-and-begin)))

  (advice-add #'doom-themes-neotree-insert-dir :override #'doom-themes-neotree-insert-dir-a))


(provide 'init-neotree)

;;; init-neotree.el ends here

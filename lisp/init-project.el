;; init-project.el --- project configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  project configurations
;;

;;; Code:

;; (use-package counsel-projectile
;;   :hook (after-init . counsel-projectile-mode))

(defun +project/lsp-project-root (&optional dir)
  (let* ((cur-dir (or dir (expand-file-name default-directory)))
         (lsp-folders (lsp-session-folders (lsp-session)))
         (r (find-if (lambda (path) (string-prefix-p path cur-dir)) lsp-folders)))
    r))

(use-package projectile
  :hook (after-init . projectile-mode)
  :bind ("C-<tab>" . projectile-next-project-buffer)
  :config
  ;; switch project to project root dir instead of project file
  (setq projectile-switch-project-action #'projectile-dired)
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  ;; cache file in ~/.emacs.d/projectile.cache
  (setq projectile-enable-caching t))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-functions #'+project/lsp-project-root)

  (defun +project/projectile-buffer-filter (buffer)
    (let ((name (buffer-name buffer)))
      (or (and (string-prefix-p "*" name)
               (not (string-prefix-p "*eww*" name))
               (not (string-prefix-p "*ein: http" name))
               (not (string-prefix-p "*ein:notebooklist" name))
               (not (string-prefix-p "*vterm:" name)))
          (string-match-p "magit.*:" name)
          (equal (buffer-name (current-buffer)) name))))

  (defun +project/projectile-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (buffer) (+project/projectile-buffer-filter buffer))
     buffers))

  (setq projectile-buffers-filter-function #'+project/projectile-buffer-filter-function)

  (defun +projectile/ivy-switch-buffer ()
    (interactive)
    (ivy-read "Switch to buffer: "
              (delete (buffer-name (current-buffer))
                      (when (projectile-project-root)
                        (projectile-project-buffer-names)))
              :initial-input nil
              :action #'ivy--switch-buffer-action
              :caller '+projectile/ivy-switch-buffer)))

(use-package find-file-in-project
  :commands (find-file-in-project
             find-file-in-current-directory
             find-file-in-project-not-ignore)
  :config
  (setq ffip-project-root-function #'+project/lsp-project-root)

  ;; A simple, fast and user-friendly alternative to 'find'
  ;; https://github.com/sharkdp/fd
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t))

  (defun find-file-in-project-not-ignore ()
    (interactive)
    (let ((ffip-rust-fd-respect-ignore-files nil))
      (find-file-in-project)))

  ;; Temporarily fix 'find-file-in-current-directory level is nil problem
  (defun ffip-parent-directory (level directory)
    "Return LEVEL up parent directory of DIRECTORY."
    (let* ((rlt directory))
      (while (and level (> level 0) (not (string= "" rlt)))
        (setq rlt (file-name-directory (directory-file-name rlt)))
        (setq level (1- level)))
      (if (string= "" rlt) (setq rlt nil))
      rlt)))

;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

(use-package persp-mode
  :defer t)


(provide 'init-project)

;;; init-project.el ends here

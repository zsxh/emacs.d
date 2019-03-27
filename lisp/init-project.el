;; init-project.el --- project configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  project configurations
;;

;;; Code:

;; (use-package counsel-projectile
;;   :ensure t
;;   :hook (after-init . counsel-projectile-mode))

(use-package projectile
  :ensure t
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
  (defun +project/projectile-buffer-filter (buffer)
    (let ((name (buffer-name buffer)))
      (or (string-prefix-p "*" name)
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
                      (projectile-project-buffer-names))
              :initial-input nil
              :action #'ivy--switch-buffer-action
              :caller '+projectile/ivy-switch-buffer)))


(use-package find-file-in-project
  :ensure t
  :commands (find-file-in-project
             find-file-in-current-directory
             find-file-in-project-not-ignore)
  :config
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
  :ensure t
  :defer t)


(provide 'init-project)

;;; init-project.el ends here

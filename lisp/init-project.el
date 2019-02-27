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
  (defun +project/projectile-buffer-filter (name)
    (or (string-prefix-p "*" name)
        (and
         (string-prefix-p "magit" name)
         (not (file-name-extension name)))
        (equal (buffer-name (current-buffer)) name)))

  (defun +project/projectile-buffer-filter-function (buffers)
    (cl-remove-if
     (lambda (b) (+project/projectile-buffer-filter (buffer-name b)))
     buffers))

  (setq projectile-buffers-filter-function #'+project/projectile-buffer-filter-function))

;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

(use-package persp-mode
  :ensure t
  :defer t)


(provide 'init-project)

;;; init-project.el ends here

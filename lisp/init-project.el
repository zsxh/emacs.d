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
  :config
  ;; switch project to project root dir instead of project file
  (setq projectile-switch-project-action #'projectile-dired)
  (with-eval-after-load 'ivy
    (setq projectile-completion-system 'ivy))
  ;; cache file in ~/.emacs.d/projectile.cache
  (setq projectile-enable-caching t))


;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

(use-package persp-mode
  :ensure t
  :defer t)


(provide 'init-project)

;;; init-project.el ends here

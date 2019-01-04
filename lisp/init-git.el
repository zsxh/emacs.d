;; init-git.el --- Version Control Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Version Control Configuations
;;

;;; Code:

(use-package magit
  :commands (magit magit-blame magit-file-popup)
  :defer 10
  :ensure t
  :config
  ;; https://github.com/magit/magit/issues/2371#issuecomment-152746346
  ;; value nil, vc mode-line update when buffer changed. t, update every auto-revert-interval seconds
  ;; (setq auto-revert-check-vc-info t)
  )

(use-package evil-magit
  :ensure t
  :after magit
  :config
  (evil-define-minor-mode-key 'normal 'with-editor-mode
    ",c" 'with-editor-finish
    ",k" 'with-editor-cancel)
  (evil-define-minor-mode-key 'normal 'magit-blame-mode
    "q" 'magit-blame-quit
    "c" 'magit-blame-cycle-style))

;; https://github.com/alphapapa/magit-todos
(use-package magit-todos
  :ensure t
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-auto-group-items 'always)
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'magit-todos)))

;; TODO: config forge and github, gitlab ...
;; (use-package forge
;;   :ensure t
;;   :after magit)


(provide 'init-git)

;;; init-git.el ends here

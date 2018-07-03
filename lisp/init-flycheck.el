;; init-flycheck --- flycheck configurations -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  ;; :commands flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (use-package flycheck-pos-tip
        :ensure t
        :hook (after-init . flycheck-pos-tip-mode)
        :config (setq flycheck-pos-tip-timeout 30))
    (use-package flycheck-popup-tip
      :ensure t
      :hook (after-init . flycheck-popup-tip-mode)))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :ensure t
    :hook (after-init . avy-flycheck-setup))
  )

(use-package flycheck-posframe
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))


(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here

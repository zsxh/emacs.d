;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

;; Group ibuffer's list by project root
;; (use-package ibuffer-projectile
;;   :ensure t
;;   :bind (("C-x C-b" . ibuffer)
;;          ;; :map ibuffer-mode-map
;;          ;; ("j" . ibuffer-forward-line)
;;          ;; ("k" . ibuffer-backward-line)
;;          )
;;   :hook ((ibuffer . ibuffer-auto-mode)
;;          (ibuffer . ibuffer-projectile-set-filter-groups)
;;          (ibuffer . (lambda ()
;;                       (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                         (ibuffer-do-sort-by-alphabetic)))))
;;   :config (setq ibuffer-filter-group-name-face 'font-lock-function-name-face))

(use-package ibuffer-vc
  :ensure t
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer . (lambda ()
                      (ibuffer-vc-set-filter-groups-by-vc-root)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here

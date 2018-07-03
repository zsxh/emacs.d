;; -*- lexical-binding: t -*-

;; ivy
(use-package ivy
  :ensure t
  :bind (:map ivy-minibuffer-map
         ("C-k" . ivy-previous-line)
         ("C-j" . ivy-next-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;; counsel
(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)))

;; ivy-posframe
(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-display-function #'ivy-posframe-display)
  (ivy-posframe-enable))



(provide 'init-ivy)

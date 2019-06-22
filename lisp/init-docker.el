;; init-docker.el --- Emacs integration for Docker	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs integration for Docker
;;

;;; Code:

;; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :commands docker)

(with-eval-after-load 'evil
  (with-eval-after-load 'tablist
    (evil-define-key 'normal tablist-minor-mode-map
      "m" 'tablist-mark-forward
      "u" 'tablist-unmark-forward
      "t" 'tablist-toggle-marks)))

(use-package docker-compose-mode
  :ensure t
  :commands docker-compose-mode)


(provide 'init-docker)

;;; init-docker.el ends here

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

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode
  :config
  (+funcs/set-leader-keys-for-major-mode
   dockerfile-mode-map
   "b" '(dockerfile-build-buffer :which-key "build-buffer")
   "B" '(dockerfile-build-no-cache-buffer :which-key "build-no-cache-buffer")))

(use-package docker-compose-mode
  :ensure t
  :commands docker-compose-mode)

(with-eval-after-load 'tablist
  (with-eval-after-load 'evil
    (evil-define-key 'normal tablist-minor-mode-map
      "m" 'tablist-mark-forward
      "u" 'tablist-unmark-forward
      "t" 'tablist-toggle-marks)))


(provide 'init-docker)

;;; init-docker.el ends here

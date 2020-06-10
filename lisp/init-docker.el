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
  :commands docker
  :config
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-processes.html
  ;; On remote hosts, the local `shell-file-name' might be useless, use `explicit-shell-file-name' instead instead
  ;; fixed /usr/bin/zsh not found error
  (defun docker-container-shell (container &optional read-shell)
    "Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it."
    (interactive (list
                  (docker-container-read-name)
                  current-prefix-arg))
    (let* ((explicit-shell-file-name (docker-container--read-shell read-shell))
           (container-address (format "docker:%s:/" container))
           (file-prefix (let ((prefix (file-remote-p default-directory)))
                          (if prefix
                              (format "%s|" (s-chop-suffix ":" prefix))
                            "/")))
           (default-directory (format "%s%s" file-prefix container-address)))
      (shell (docker-generate-new-buffer "shell" default-directory)))))

(use-package dockerfile-mode
  :commands dockerfile-mode
  :config
  (+funcs/major-mode-leader-keys
   dockerfile-mode-map
   "b" '(dockerfile-build-buffer :which-key "build-buffer")
   "B" '(dockerfile-build-no-cache-buffer :which-key "build-no-cache-buffer")))

(use-package docker-compose-mode
  :commands docker-compose-mode)

(with-eval-after-load 'tablist
  (with-eval-after-load 'evil
    (evil-define-key 'normal tablist-minor-mode-map
      "m" 'tablist-mark-forward
      "u" 'tablist-unmark-forward
      "t" 'tablist-toggle-marks)))

;; TODO: k8s configs
(use-package kubernetes
  :if (executable-find "kubectl")
  :commands (kubernetes-overview))


(provide 'init-docker)

;;; init-docker.el ends here

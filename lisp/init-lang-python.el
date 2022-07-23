;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; TODO: pyright language server
(use-package python
  :ensure nil
  :hook ((python-mode . lsp-bridge-mode)
         (python-mode . pyenv-mode))
  :custom (python-indent-offset 2)
  :config
  (+lsp/set-leader-keys python-mode-map))

;; https://github.com/manateelazycat/lsp-bridge/wiki/Python-virtualenv
(defun local/lsp-bridge-get-lang-server-by-project (project-path filepath)
  (let* ((json-object-type 'plist)
         (custom-dir (expand-file-name "cache/lsp-bridge-pyright" user-emacs-directory))
         (custom-config (expand-file-name "pyright.json" custom-dir))
         (default-config (json-read-file (expand-file-name "submodules/lsp-bridge/langserver/pyright.json" user-emacs-directory)))
         (settings (plist-get default-config :settings)))

    (plist-put settings :pythonPath (executable-find "python"))

    (make-directory (file-name-directory custom-config) t)

    (with-temp-file custom-config
      (insert (json-encode default-config)))

    custom-config))

(add-hook 'python-mode-hook (lambda () (setq-local lsp-bridge-get-lang-server-by-project 'local/lsp-bridge-get-lang-server-by-project)))

(use-package pyvenv
  :commands pyvenv-activate)

(add-hook 'pyvenv-post-activate-hooks
          (lambda ()
            (lsp-bridge-restart-process)))

;; Setup PYENV_VERSION environment variable and
;; `python-shell-virtualenv-root' custom variable based on user input
(use-package pyenv-mode
  :commands pyenv-mode)


(provide 'init-lang-python)

;;; init-lang-python.el ends here

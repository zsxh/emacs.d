;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; NOTE: `PDM' https://chriswarrick.com/blog/2023/01/15/how-to-improve-python-packaging/

;; NOTE: Install: pyright, ruff-lsp
;; pip install --user pyright ruff-lsp --upgrade

;; TODO: pyright custom type stub
;; settings python.analysis.stubPath, https://github.com/microsoft/pyright/blob/main/docs/settings.md
;; customize type stub, https://github.com/microsoft/python-type-stubs

;; Use `+python/create-project-lsp-config' to create pyright,ruff-lsp config for each project
(use-package python
  :ensure nil
  :init
  (defun +python/enable-lsp ()
    (when-let* ((dir (+python/user-languageserver-config-dir)))
      (setq-local lsp-bridge-user-langserver-dir dir
                  lsp-bridge-user-multiserver-dir dir))
    (lsp-bridge-mode))
  :hook (python-base-mode . +python/enable-lsp)
  :custom (python-indent-offset 2)
  :config
  (+lsp/set-leader-keys python-mode-map)
  (+lsp/set-leader-keys python-ts-mode-map))

;; Python virtual environment support for Emacs
(use-package pyvenv
  :commands pyvenv-activate)

;; Setup `PYENV_VERSION' environment variable and `python-shell-virtualenv-root' custom variable based on user input
(use-package pyenv-mode
  :commands pyenv-mode)

(defun +lsp/read-default-singleserver-config (server)
  (let* ((json-object-type 'plist)
         (server-config-dir (expand-file-name "submodules/lsp-bridge/langserver" user-emacs-directory))
         (server-config (concat server-config-dir "/" server ".json")))
    (json-read-file server-config)))

(defun +python/user-languageserver-config-dir ()
  (when-let* ((root (+project/root)))
    (expand-file-name ".lsp-bridge" root)))

(defun +python/create-project-lsp-config (python)
  "Create pyright, ruff-lsp configs for current project."
  (interactive (let ((current-python (executable-find "python")))
                 (list (read-file-name "Python Interpreter: "
                                       (directory-file-name current-python) current-python))))
  (let* ((json-object-type 'plist)
         (pyright-server-name "pyright-background-analysis")
         (ruff-server-name "ruff")
         (pyright-config (+lsp/read-default-singleserver-config pyright-server-name))
         (ruff-config (+lsp/read-default-singleserver-config ruff-server-name))
         (python-interpreter (expand-file-name python))
         (ruff-path (executable-find "ruff"))
         (project-config-dir (+python/user-languageserver-config-dir))
         (pyright-config-file (expand-file-name (concat pyright-server-name ".json") project-config-dir))
         (ruff-config-file (expand-file-name (concat ruff-server-name ".json") project-config-dir)))
    ;; update pyright config
    (let ((settings (plist-get pyright-config :settings)))
      (plist-put settings :pythonPath python-interpreter))
    ;; update ruff config
    (let* ((initializationOptions (plist-get ruff-config :initializationOptions))
           (settings (plist-get initializationOptions :settings)))
      (plist-put settings :interpreter (vector python-interpreter))
      (plist-put settings :path (vector ruff-path))
      settings)
    ;; save config
    (unless (file-exists-p project-config-dir)
      (mkdir project-config-dir t))
    (with-temp-file pyright-config-file
      (insert (json-encode pyright-config)))
    (with-temp-file ruff-config-file
      (insert (json-encode ruff-config)))))


(provide 'init-lang-python)

;;; init-lang-python.el ends here

;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

(require 'init-language-server)

(use-package python
  :ensure nil
  :defer t
  :hook (python-mode . lsp-deferred)
  :custom (python-indent-offset 2))

;; something you might interested:
;; Using lsp-python-ms with anaconda
;; https://github.com/emacs-lsp/lsp-python-ms/issues/53
(use-package lsp-python-ms
  :after python)

;; The main entry points are `pyvenv-activate', which queries the user for a virtual environment directory
;; to activate, and `pyvenv-workon', which queries for a virtual environment in $WORKON_HOME (from virtualenvwrapper.sh).
(use-package pyvenv
  :commands pyvenv-activate)

;; Setup PYENV_VERSION environment variable and
;; `python-shell-virtualenv-root' custom variable based on user input
(use-package pyenv-mode
  :commands pyenv-mode
  :hook (python-mode . pyenv-mode))

(with-eval-after-load 'python
  (defun +python/set-leader-keys ()
    (+language-server/set-common-leader-keys python-mode-map)
    (+funcs/major-mode-leader-keys
     python-mode-map
     "'" '(+python/repl-vterm :which-key "repl")
     "c" '(nil :which-key "compile-exec")
     "cc" '(+python/python-execute-file :which-key "execute-file")
     "cC" '(+python/python-execute-file-focus :which-key "execute-file-focus")))

  (defun +python/pyenv-executable-find (command)
    (executable-find command))

  (defun +python/python-setup-shell (&rest args)
    (if (+python/pyenv-executable-find "ipython")
        (progn (setq python-shell-interpreter "ipython")
               (if (version< (replace-regexp-in-string "[\r\n|\n]$" "" (shell-command-to-string "ipython --version")) "5")
                   (setq python-shell-interpreter-args "-i")
                 (setq python-shell-interpreter-args "--simple-prompt -i")))
      (progn
        (setq python-shell-interpreter-args "-i")
        (setq python-shell-interpreter "python"))))

  (defun +python/python-toggle-breakpoint ()
    "Add a break point, highlight it."
    (interactive)
    (let ((trace (cond ((+python/pyenv-executable-find "trepan3k") "import trepan.api; trepan.api.debug()")
                       ((+python/pyenv-executable-find "wdb") "import wdb; wdb.set_trace()")
                       ((+python/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                       ((+python/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
                       ((+python/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                       ((+python/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
                       (t "import pdb; pdb.set_trace()")))
          (line (thing-at-point 'line)))
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (insert "\n")
          (python-indent-line)))))

  (defun +python/repl ()
    "Open the Python REPL."
    (interactive)
    ;; (process-buffer (run-python nil t t))
    (pop-to-buffer (process-buffer (python-shell-get-or-create-process)))
    (evil-insert-state))

  (defun +python/repl-vterm ()
    "Executing ipython/python in project virtual environment,
virtual environment path should be 'venv' in project root."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (with-current-buffer (vterm-other-window)
        (when (file-exists-p (expand-file-name "venv" (projectile-project-root)))
          (dolist (char (string-to-list "source venv/bin/activate"))
            (vterm--update vterm--term (char-to-string char) nil nil nil))
          (vterm-send-return))
        (let ((py-interpreter (cond ((file-exists-p (expand-file-name "venv/bin/ipython" (projectile-project-root))) "ipython")
                                    (t "python"))))
          (dolist (char (string-to-list py-interpreter))
            (vterm--update vterm--term (char-to-string char) nil nil nil))
          (vterm-send-return)))))

  (defun +python/python-execute-file (arg)
    "Execute a python script in a shell."
    (interactive "P")
    ;; set compile command to buffer-file-name
    ;; universal argument put compile buffer in comint mode
    (let ((universal-argument t)
          (compile-command (format "%s %s"
                                   (+python/pyenv-executable-find python-shell-interpreter)
                                   (file-name-nondirectory buffer-file-name))))
      (if arg
          (call-interactively 'compile)
        (compile compile-command t)
        (with-current-buffer (get-buffer "*compilation*")
          ;; python-shell--interpreter default value is void...
          ;; Fix inferior-python-mode initialization error
          (setq python-shell--interpreter nil)
          (setq python-shell--interpreter-args nil)
          (inferior-python-mode)))))

  (defun +python/python-execute-file-focus (arg)
    "Execute a python script in a shell and switch to the shell buffer in
 'normal state'."
    (interactive "P")
    (+python/python-execute-file arg)
    (switch-to-buffer-other-window "*compilation*")
    (end-of-buffer)
    (evil-normal-state))

  (+python/python-setup-shell)
  (+python/set-leader-keys))


(provide 'init-lang-python)

;;; init-lang-python.el ends here

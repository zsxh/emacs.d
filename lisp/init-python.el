;; init-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; Somehow pipenv cant really active virtulenv, but i found pyvenv do a good job for this
(use-package pyvenv
  :ensure t
  :commands pyvenv-activate)

(use-package pyenv-mode
  :ensure t
  :commands pyenv-mode
  :hook (python-mode . pyenv-mode))

;; https://emacs-china.org/t/emacs-pipenv-el-make-process-color-code-0m/7614
(use-package pipenv
  :ensure t
  :commands pipenv-mode
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(require 'init-language-server)

(with-eval-after-load 'python
  (setq-local python-indent-offset 2)
  (+python/python-setup-shell)
  (+python/set-leader-keys)
  (with-eval-after-load 'dap-mode
    (use-package dap-python)))

;; Install https://github.com/andrew-christianson/lsp-python-ms/
;; cd ~/.emacs.d
;; git clone https://github.com/Microsoft/python-language-server.git --depth 1
;; cd python-language-server/src/LanguageServer/Impl
;; dotnet build -c Release -r linux-x64

(setq +python/ms-python-language-server-dir
      (expand-file-name "python-language-server" user-emacs-directory))

;; for dev build of language server
(setq lsp-python-ms-dir
      (expand-file-name "output/bin/Release/" +python/ms-python-language-server-dir))

;; for executable of language server, if it's not symlinked on your PATH
(setq lsp-python-ms-executable
      (expand-file-name "Microsoft.Python.LanguageServer.LanguageServer" lsp-python-ms-dir))

(use-package lsp-python-ms
  :if (file-executable-p lsp-python-ms-executable)
  :after lsp-mode
  :quelpa ((lsp-python-ms :fetcher github :repo "andrew-christianson/lsp-python-ms")))

(add-hook 'python-mode-hook 'lsp)

(defun +python/set-leader-keys ()
  (+funcs/set-leader-keys-for-major-mode
   'python-mode-map
   "'"  '(+python/repl :which-key "repl")
   "c"  '(nil :which-key "compile-exec")
   "cc" '(+python/python-execute-file :which-key "execute-file")
   "cC" '(+python/python-execute-file-focus :which-key "execute-file-focus")
   "d"  '(nil :which-key "debug")
   "db" '(dap-breakpoint-toggle :which-key "breakpoint")
   "dB" '(dap-breakpoint-condition :which-key "breakpoint-condition")
   "dr" '(dap-debug :which-key "debug")))

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

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  ;; (process-buffer (run-python nil t t))
  (pop-to-buffer (process-buffer (python-shell-get-or-create-process)))
  (evil-insert-state))

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


(provide 'init-python)

;;; init-python.el ends here

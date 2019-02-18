;; init-java.el --- Java Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Java Configurations
;;

;;; Code:

(require 'init-language-server)

(use-package lsp-java
  :after lsp-mode
  :quelpa ((lsp-java :fetcher github :repo "emacs-lsp/lsp-java"))
  :after (require 'cl))

(add-hook 'java-mode-hook 'lsp)

(with-eval-after-load 'cc-mode
  (+funcs/set-leader-keys-for-major-mode
   'java-mode-map
   "c" '(+java/compile :which-key "compile")
   "d" '(nil :which-key "debug")
   "db" '(dap-breakpoint-toggle :which-key "breakpoint")
   "dB" '(dap-breakpoint-condition :which-key "breakpoint-condition")
   "dr" '(dap-java-debug :which-key "debug")
   "dt" '(dap-java-debug-test-method :which-key "debug-junit-test-method")
   "dT" '(dap-java-debug-test-class :which-key "debug-junit-class")
   "dk" '(+dap/debug-key-settings--toggle :which-key "toggle-debug-keybindings")
   "i" '(nil :which-key "implement")
   "ic" '(lsp-java-add-import :which-key "import-class")
   "im" '(lsp-java-add-unimplemented-methods :which-key "add-unimplemented-methods")
   "j" '(+java/set-jdk :which-key "set-jdk")
   "r" '(nil :which-key "run")
   "rt" '(dap-java-run-test-method :which-key "run-junit-test-method")
   "rT" '(dap-java-run-test-class :which-key "run-junit-class")))

(defvar jdks-installed-dir "/usr/local/"
  "JDKs intalled directory.")

;;;###autoload
(defun +java/set-jdk (jdk-version)
  "Select JDK-VERSION in `jdks-installed-dir' as global version,
JDK-VERSION directory name prefix \"jdk-\" is required,
`jdks-installed-dir'/jdk/bin in $PATH is required."
  (interactive (list (completing-read "JDK-version: " (+java/list-jdk-version))))
  (let ((target (expand-file-name jdk-version jdks-installed-dir))
        (link-name (expand-file-name "jdk" jdks-installed-dir)))
    (+funcs/sudo-shell-command (concat "ln -nsf " target " " link-name))))

;;;###autoload
(defun +java/list-jdk-version ()
  "Find all jdks which start with \"jdk-\" in `jdks-installed-dir'"
  (let ((files (directory-files jdks-installed-dir))
        (result nil))
    (dolist (file files)
      (if (string-match "jdk-" file)
          (push file result)))
    result))

;;;###autoload
(defun +java/compile ()
  (interactive)
  (let ((default-directory (projectile-project-root))
        (compile-command "mvn clean compile test-compile"))
    (compile compile-command)))


(provide 'init-java)

;;; init-java.el ends here

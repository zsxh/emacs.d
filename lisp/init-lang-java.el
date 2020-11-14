;; init-lang-java.el --- Java Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Java Configurations
;;

;;; Code:

(require 'init-lsp)

(use-package lsp-java
  :quelpa (lsp-java :fetcher github :repo "emacs-lsp/lsp-java")
  :defer t
  :preface
  (setq lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file ".cache/java-workspace/"))
        lsp-java-inhibit-message t)
  (let ((java-format-style-file (expand-file-name (locate-user-emacs-file ".cache/eclipse-java-google-style.xml"))))
    (when (file-exists-p java-format-style-file)
      ;; https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings
      ;; I prefer {join_wrapped_lines : false}
      (setq lsp-java-format-settings-url java-format-style-file
            lsp-java-format-settings-profile "GoogleStyle")))
  :config
  (require 'helm nil t)
  ;; (require 'lsp-java-boot)
  ;; (setq lsp-java-boot-enabled nil)

  ;; check this out, https://github.com/emacs-lsp/lsp-java/issues/54#issuecomment-553995773
  (let ((lombok-jar (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar")))
    (when (file-exists-p lombok-jar)
      (setq lsp-java-vmargs
            `("-noverify"
              "-Xmx1G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              ;; ,(concat "-DproxyHost=" personal-proxy-http-host)
              ;; ,(format "-DproxyPort=%s" personal-proxy-http-port)
              ,(concat "-javaagent:" lombok-jar))))))

(use-package dap-java
  :after lsp-java
  :ensure lsp-java
  :config
  (require 'dap-java)
  (setq dap-java-test-runner (expand-file-name
                              (concat lsp-java-server-install-dir
                                      "test-runner/junit-platform-console-standalone.jar"))
        dap-java-default-debug-port 5005))

(with-eval-after-load 'cc-mode
  ;; FIXME: when I put these codes in +java/setup, i have to toggle emacs/evil mode to activate keybindings, i think it's a bug of evil mode
  (+language-server/set-common-leader-keys java-mode-map)

  (+funcs/major-mode-leader-keys
   java-mode-map
   "dr" '(dap-java-debug :which-key "run")
   "dR" '(dap-debug :which-key "run-attach")
   "dt" '(dap-java-debug-test-method :which-key "debug-junit-test-method")
   "dT" '(dap-java-debug-test-class :which-key "debug-junit-class")
   "i" '(nil :which-key "implement")
   "ic" '(lsp-java-add-import :which-key "import-class")
   "im" '(lsp-java-add-unimplemented-methods :which-key "add-unimplemented-methods")
   "ig" '(lsp-java-generate-getters-and-setters :which-key "generate-getters-and-setters")
   "j" '(+java/set-jdk :which-key "set-jdk")
   "r" '(nil :which-key "run")
   "rt" '(dap-java-run-test-method :which-key "run-junit-test-method")
   "rT" '(dap-java-run-test-class :which-key "run-junit-class")))

(add-hook-run-once 'java-mode-hook '+java/setup)

(defun +java/setup ()
  (require 'lsp-java)
  (add-hook 'java-mode-hook 'lsp-deferred)
  ;; (add-hook 'lsp-after-open-hook
  ;;           (lambda ()
  ;;             (when (eq major-mode 'java-mode)
  ;;               (lsp-java-lens-mode)
  ;;               (lsp-java-boot-lens-mode))))
  (setq-local lsp-completion-show-detail nil)
  (lsp-deferred)

  (defvar jdks-installed-dir "/usr/local/"
    "JDKs intalled directory.")

  (defun +java/set-jdk (jdk-version)
    "Select JDK-VERSION in `jdks-installed-dir' as global version,
JDK-VERSION directory name prefix \"jdk-\" is required,
`jdks-installed-dir'/jdk/bin in $PATH is required."
    (interactive (list (completing-read "JDK-version: " (+java/list-jdk-version))))
    (let ((target (expand-file-name jdk-version jdks-installed-dir))
          (link-name (expand-file-name "jdk" jdks-installed-dir)))
      (+funcs/sudo-shell-command (concat "ln -nsf " target " " link-name))))

  (defun +java/list-jdk-version ()
    "Find all jdks which start with \"jdk-\" in `jdks-installed-dir'"
    (let ((files (directory-files jdks-installed-dir))
          (result nil))
      (dolist (file files)
        (if (string-match "\\(graalvm\\|jdk\\)-" file)
            (push file result)))
      result))

  (defun +java/compile ()
    (interactive)
    (let ((default-directory (+project/root))
          (compile-command "mvn clean compile test-compile"))
      (compile compile-command)
      (switch-to-buffer-other-window "*compilation*")
      (end-of-buffer))))

(with-eval-after-load 'lsp-java
  ;; install lsp java server via http proxy
  (when (and personal-proxy-http-host personal-proxy-http-port)
    (defun lsp-java--ensure-server-a (_client callback error-callback _update?)
      "Ensure that JDT server and the other configuration."
      (let* ((default-directory (make-temp-file "lsp-java-install" t))
             (installed-mvn (let ((mvn-executable (executable-find "mvn")))
                              ;; Quote path to maven executable if it has spaces.
                              (if (and mvn-executable
                                       (string-match "\s" mvn-executable))
                                  (format "\"%s\"" mvn-executable)
                                mvn-executable)))
             (mvn-command-and-options (if installed-mvn
                                          (list installed-mvn)
                                        (lsp-java--prepare-mvnw)))
             (other-options
              (list (format "-Djdt.js.server.root=%s"
                            (expand-file-name lsp-java-server-install-dir))
                    (format "-Djunit.runner.root=%s"
                            (expand-file-name
                             (if (boundp 'dap-java-test-runner)
                                 (file-name-directory dap-java-test-runner)
                               (concat (file-name-directory lsp-java-server-install-dir)
                                       "test-runner"))))
                    (format "-Djunit.runner.fileName=%s"
                            (if (boundp 'dap-java-test-runner)
                                (file-name-nondirectory (directory-file-name dap-java-test-runner))
                              "junit-platform-console-standalone.jar"))
                    (format "-Djava.debug.root=%s"
                            (expand-file-name (lsp-java--bundles-dir)))
                    "clean"
                    "package"
                    (format "-Djdt.download.url=%s" lsp-java-jdt-download-url)
                    (format "-DproxyHost=%s" personal-proxy-http-host)
                    (format "-DproxyPort=%s" personal-proxy-http-port))))
        (url-copy-file (concat lsp-java--download-root "pom.xml") "pom.xml" t)
        (apply #'lsp-async-start-process
               callback
               error-callback
               (append mvn-command-and-options other-options))))

    (advice-add 'lsp-java--ensure-server :override 'lsp-java--ensure-server-a)))

(add-hook-run-once
 'pom-xml-mode-hook
 (lambda nil
   (setq +web/pom-formatter-buffer-name "*format pom xml*")

   (add-to-list 'display-buffer-alist
                `(,+web/pom-formatter-buffer-name . (display-buffer-no-window . nil)))

   (defun +java/sortpom-formatter ()
     (interactive)
     (let ((output-buffer (get-buffer-create +web/pom-formatter-buffer-name))
           (cmd (s-join " " '("mvn"
                              "com.github.ekryd.sortpom:sortpom-maven-plugin:sort"
                              "-Dsort.keepBlankLines"
                              "-Dsort.predefinedSortOrder=custom_1"
                              "-Dsort.createBackupFile=false"))))
       (async-shell-command cmd output-buffer)))

   (+funcs/major-mode-leader-keys pom-xml-mode-map
                                  "f" '(+java/sortpom-formatter :which-key "format"))))


(provide 'init-lang-java)

;;; init-lang-java.el ends here

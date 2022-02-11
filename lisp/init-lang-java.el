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
  ;; :quelpa (lsp-java :fetcher github :repo "emacs-lsp/lsp-java")
  :defer t
  :preface
  (setq lsp-java-workspace-dir (expand-file-name (locate-user-emacs-file "cache/java-workspace/"))
        lsp-java-inhibit-message t
        ;; https://github.com/dgileadi/vscode-java-decompiler
        lsp-java-content-provider-preferred "fernflower"
        lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz")

  (let ((java-format-style-file (expand-file-name (locate-user-emacs-file "cache/eclipse-java-google-style.xml"))))
    (when (file-exists-p java-format-style-file)
      ;; https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings
      ;; I prefer {join_wrapped_lines : false}
      (setq lsp-java-format-settings-url java-format-style-file
            lsp-java-format-settings-profile "GoogleStyle")))
  :config
  (setq lsp-java-completion-overwrite nil
        lsp-java-folding-range-enabled nil
        lsp-java-progress-reports-enabled nil
        lsp-java-format-comments-enabled nil
        lsp-java-signature-help-enabled nil
        ;; Set a small num to improve performance
        lsp-java-completion-max-results 30
        lsp-java-selection-enabled nil
        lsp-java-selection-range-enabled nil
        ;; NOTE: https://github.com/redhat-developer/vscode-java/issues/406#issuecomment-356303715
        ;; > We enabled it by default so that workspace-wide errors can be reported (eg. removing a public method in one class would cause compilation errors in other files consuming that method).
        ;; for large workspaces, it may make sense to be able to disable autobuild if it negatively impacts performance.
        lsp-java-autobuild-enabled nil
        ;; JAVA Tooling JDK, lsp server require java 11+
        ;; https://github.com/redhat-developer/vscode-java/#java-tooling-jdk
        lsp-java-java-path "~/.jenv/versions/11/bin/java"
        ;; lsp-java-java-path "~/.jenv/versions/17/bin/java"
        ;; Project JDKs
        ;; https://github.com/redhat-developer/vscode-java/#project-jdks
        ;; https://github.com/redhat-developer/vscode-java/issues/2151
        lsp-java-configuration-runtimes '[(:name "JavaSE-1.8" :path "/usr/local/jdk-8")
                                          (:name "JavaSE-11" :path "/usr/local/graalvm-ce-java11-22.0.0.2")
                                          (:name "JavaSE-17" :path "/usr/local/graalvm-ce-java17-22.0.0.2" :default t)])

  ;; check this out, https://github.com/emacs-lsp/lsp-java/issues/54#issuecomment-553995773
  (let ((lombok-jar (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar")))
    (when (file-exists-p lombok-jar)
      ;; current VSCode default, https://github.com/redhat-developer/vscode-java/blob/master/package.json#L156
      (setq lsp-java-vmargs `("-XX:+UseParallelGC"
                              "-XX:GCTimeRatio=4"
                              "-XX:AdaptiveSizePolicyWeight=90"
                              "-Dsun.zip.disableMemoryMapping=true"
                              "-Xmx4G"
                              "-Xms200m"
                              ;; "-XX:+UnlockExperimentalVMOptions"
                              ;; "-XX:+UseZGC"
                              ;; ,(concat "-DproxyHost=" personal-proxy-http-host)
                              ;; ,(format "-DproxyPort=%s" personal-proxy-http-port)
                              ,(concat "-javaagent:" lombok-jar)))))
  (setq global-mode-string (delete (list '(t lsp-java-progress-string)) global-mode-string)))

(defun +java/setup ()
  (require 'lsp-java)
  (require 'lsp-java-boot)
  ;; TODO: project specified/dir-locals `lsp-java-boot-enabled'
  ;; (setq lsp-java-boot-enabled nil)

  (let ((f (lambda ()
             (setq-local lsp-completion-show-detail nil
                         lsp-completion-no-cache nil
                         company-minimum-prefix-length 2
                         auto-save-idle 3)
             (lsp-deferred))))
    (add-hook 'java-mode-hook f)
    (add-hook 'lsp-configure-hook
              (lambda ()
                (when (eq major-mode 'java-mode)
                  (lsp-lens-mode)
                  (lsp-java-lens-mode))))
    (funcall f)))

(add-hook-run-once 'java-mode-hook '+java/setup)

(with-eval-after-load 'conf-mode
  (require 'lsp-java-boot)
  (add-hook 'conf-javaprop-mode-hook 'lsp-deferred))

(with-eval-after-load 'yaml-mode
  (require 'lsp-java-boot)
  (add-hook 'yaml-mode-hook 'lsp-deferred))

;; NOTE: debug template args `vmArgs', `noDebug'...
;; git clone https://github.com/microsoft/java-debug code base to checkout extra debug args, like `vmArgs'
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
   "B" '(lsp-java-build-project :which-key "lsp-java-build-project")
   "dr" '(dap-java-debug :which-key "dap-java-debug")
   "dR" '(dap-debug :which-key "dap-debug")
   "dt" '(dap-java-debug-test-method :which-key "debug-junit-test-method")
   "dT" '(dap-java-debug-test-class :which-key "debug-junit-class")
   "i" '(nil :which-key "implement")
   "ic" '(lsp-java-add-import :which-key "import-class")
   "im" '(lsp-java-add-unimplemented-methods :which-key "add-unimplemented-methods")
   "ig" '(lsp-java-generate-getters-and-setters :which-key "generate-getters-and-setters")
   "ld" '(lsp-treemacs-java-deps-list :which-key "lsp-treemacs-java-deps-list")
   "r" '(nil :which-key "run")
   "rt" '(dap-java-run-test-method :which-key "run-junit-test-method")
   "rT" '(dap-java-run-test-class :which-key "run-junit-class")))

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
                              ;; "-Dsort.nrOfIndentSpace=4"
                              "-Dsort.keepBlankLines"
                              "-Dsort.predefinedSortOrder=custom_1"
                              "-Dsort.createBackupFile=false"))))
       (async-shell-command cmd output-buffer)))

   (+funcs/major-mode-leader-keys pom-xml-mode-map
                                  "f" '(+java/sortpom-formatter :which-key "format"))))

;; http://www.tianxiangxiong.com/2017/02/12/decompiling-java-classfiles-in-emacs.html
;; https://github.com/xiongtx/jdecomp
;; https://github.com/JetBrains/intellij-community/tree/master/plugins/java-decompiler/engine
;; java -cp /home/zsxh/.local/share/JetBrains/Toolbox/apps/IDEA-C/ch-0/203.7148.57/plugins/java-decompiler/lib/java-decompiler.jar org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler [-<option>=<value>]* [<source>]+ <destination>
;; TODO: `jdecomp--fernflower-decompile-file' should extract all A.class and A${anonymous}.class
(use-package jdecomp
  :commands (jdecomp-mode)
  :config
  (setq jdecomp-decompiler-type 'fernflower
        jdecomp-decompiler-paths `((fernflower . ,(expand-file-name "~/.local/share/JetBrains/Toolbox/apps/IDEA-C/ch-0/203.7148.57/plugins/java-decompiler/lib/java-decompiler.jar")))
        jdecomp-decompiler-options '((fernflower "-hes=0" "-hdc=0" "-fdi=0"))))


(provide 'init-lang-java)

;;; init-lang-java.el ends here

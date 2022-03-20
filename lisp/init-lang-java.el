;; init-lang-java.el --- Java Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Java Configurations
;;

;;; Code:

(add-hook-run-once 'java-mode-hook #'+java/eglot-setup)

(defun +java/eglot-setup ()
  (unless (featurep 'eglot)
    (require 'eglot))

  ;; https://github.com/joaotavora/eglot/discussions/868
  (add-to-list 'eglot-server-programs
               `(java-mode "jdtls"
                           "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
                           "-data" ,(expand-file-name "cache/java-workspace" user-emacs-directory)
                           ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))))

  (cl-defmethod eglot-initialization-options ((server eglot-lsp-server) &context (major-mode java-mode))
    `(:settings
      (:java
       (:configuration
        (:runtime [(:name "JavaSE-1.8" :path "/usr/local/jdk-8")
                   (:name "JavaSE-11" :path "/usr/local/graalvm-ce-java11-22.0.0.2")
                   (:name "JavaSE-17" :path "/usr/local/graalvm-ce-java17-22.0.0.2" :default t)])
        :format (:settings (:url ,(expand-file-name (locate-user-emacs-file "cache/eclipse-java-google-style.xml"))
                                 :profile "GoogleStyle"))
        ;; NOTE: https://github.com/redhat-developer/vscode-java/issues/406#issuecomment-356303715
        ;; > We enabled it by default so that workspace-wide errors can be reported (eg. removing a public method in one class would cause compilation errors in other files consuming that method).
        ;; for large workspaces, it may make sense to be able to disable autobuild if it negatively impacts performance.
        :autobuild (:enabled t)
        ;; https://github.com/dgileadi/vscode-java-decompiler
        :contentProvider (:preferred "fernflower")))
      ;; support non standard LSP `java/classFileContents', `Location' items that have a `jdt://...' uri
      ;; https://github.com/eclipse/eclipse.jdt.ls/issues/1384
      :extendedClientCapabilities (:classFileContentsSupport t)
      ;; bundles: decompilers, etc.
      ;; https://github.com/dgileadi/dg.jdt.ls.decompiler
      :bundles ,(let ((bundles-dir (expand-file-name (locate-user-emacs-file "cache/language-server/java/bundles" user-emacs-directory)))
                      jdtls-bundles)
                  (->> (when (file-directory-p bundles-dir)
                         (directory-files bundles-dir t "\\.jar$"))
                       (append jdtls-bundles)
                       (apply #'vector)))))

  (+eglot/set-leader-keys)

  (eglot-ensure)

  (add-hook 'java-mode-hook #'eglot-ensure))


;; TODO: debug template args `vmArgs', `noDebug'...
;; git clone https://github.com/microsoft/java-debug code base to checkout extra debug args, like `vmArgs'

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

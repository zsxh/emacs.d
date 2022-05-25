;; init-lang-java.el --- Java Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Java Configurations
;;

;;; Code:

;; NOTE: https://github.com/calve/flycheck-infer Flycheck for java using Infer
;; NOTE: https://fbinfer.com/ Infer
;; NOTE: https://github.com/pmd/pmd-emacs PMD Emacs
;; NOTE: https://pmd.github.io/latest/index.html PMD

;; Download http://repository.sonatype.org/service/local/artifact/maven/redirect?r=central-proxy&g=org.junit.platform&a=junit-platform-console-standalone&v=LATEST
(defvar +java/junit-platform-console-standalone-jar
  (expand-file-name (locate-user-emacs-file "cache/language-server/java/junit-console/junit-platform-console-standalone.jar")))

(add-hook-run-once 'java-mode-hook #'+java/eglot-setup)

(defun +java/eglot-setup ()
  (unless (featurep 'eglot)
    (require 'eglot))

  (defun +eglot/jdtls-contact (interactive)
    `("jdtls"
      "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
      "-data" ,(expand-file-name (file-name-base (directory-file-name (project-root (eglot--current-project))))
                                 (locate-user-emacs-file "cache/java-workspace"))
      ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))))

  (add-to-list 'eglot-server-programs '(java-mode . +eglot/jdtls-contact))

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
        :contentProvider (:preferred "fernflower"))
       :completion
       (:guessMethodArguments t
        :overwrite t
        :enabled t
        :favoriteStaticMembers ["org.junit.Assert.*"
                                "org.junit.Assume.*"
                                "org.junit.jupiter.api.Assertions.*"
                                "org.junit.jupiter.api.Assumptions.*"
                                "org.junit.jupiter.api.DynamicContainer.*"
                                "org.junit.jupiter.api.DynamicTest.*"
                                "org.mockito.Mockito.*"
                                "org.mockito.ArgumentMatchers.*"
                                "org.mockito.Answers.*"]))
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

  (+funcs/major-mode-leader-keys
   java-mode-map
   "r" '(nil :which-key "run")
   "rt" '(eglot-java-run-test :which-key "run-test-at-point")
   "rm" '(eglot-java-run-main :which-key "run-main"))

  (eglot-ensure)

  (add-hook 'java-mode-hook #'eglot-ensure))


;; https://github.com/yveszoundi/eglot-java/blob/main/eglot-java.el
(defun eglot-java--class-fqcn (&optional main-p)
  "Return the fully qualified name of a given class."
  (let* ((document-symbols (eglot-java--document-symbols))
         (package-name (eglot-java--symbol-value document-symbols "Package"))
         (class-name (eglot-java--symbol-value document-symbols "Class"))
         (method-name (unless main-p (eglot-java--method-name document-symbols class-name)))
         (package-suffix (if (string= "" package-name)
                             package-name
                           "."))
         (cls (format "%s%s%s" package-name package-suffix class-name)))

    (if method-name
        (format "%s#%s" cls method-name)
      cls)))

(defun eglot-java--symbol-value (symbols symbol-type)
  "Extract the symbol value for a given SYMBOL-TYPE from a symbol table SYMBOLS."
  (let ((symbol-details (cl-find-if
                         (lambda (elem)
                           (let* ((elem-kind (plist-get elem :kind))
                                  (elem-type (cdr (assoc elem-kind eglot--symbol-kind-names))))
                             (string= elem-type symbol-type)))
                         symbols)))
    (if symbol-details
        (plist-get symbol-details :name)
      "")))

(defun eglot-java--method-name (symbols class-name)
  (let* ((class-symbol (cl-find-if
                        (lambda (elem)
                          (string= (plist-get elem :name) class-name))
                        symbols))
         (method-symbol (cl-find-if
                         (lambda (elem)
                           (if-let* ((elem-kind (plist-get elem :kind))
                                     (elem-type (cdr (assoc elem-kind eglot--symbol-kind-names)))
                                     (method? (string= elem-type "Method"))
                                     (range (plist-get elem :range))
                                     (start (plist-get range :start))
                                     (end (plist-get range :end))
                                     (start-pos (+funcs/pos-at-line-col (plist-get start :line) (plist-get start :character)))
                                     (end-pos (+funcs/pos-at-line-col (plist-get end :line) (plist-get end :character))))
                               (<= start-pos (point) end-pos)))
                         (plist-get class-symbol :children))))
    (when method-symbol
        (let* ((range (plist-get method-symbol :selectionRange))
               (start (plist-get range :start))
               (end (plist-get range :end))
               (start-pos (+funcs/pos-at-line-col (plist-get start :line) (plist-get start :character)))
               (end-pos (+funcs/pos-at-line-col (plist-get end :line) (plist-get end :character))))
          (buffer-substring-no-properties start-pos end-pos)))))

(defun eglot-java--document-symbols ()
  "Fetch the document symbols/tokens."
  (jsonrpc-request
   (eglot--current-server-or-lose)
   :textDocument/documentSymbol
   (list :textDocument (list :uri (eglot--path-to-uri (buffer-file-name))))))


(defun eglot-java--project-classpath (filename scope)
  "Return the classpath for a given FILENAME and SCOPE."
  (plist-get (eglot-execute-command (eglot--current-server-or-lose)
                                    "java.project.getClasspaths"
                                    (vector (eglot--path-to-uri filename)
                                            (json-encode `(( "scope" . ,scope)))))
             :classpaths))

(defun eglot-java--file--test-p (file-path)
  "Tell if a file locate at FILE-PATH is a test class."
  (eglot-execute-command
   (eglot--current-server-or-lose)
   "java.project.isTestFile"
   (vector (eglot--path-to-uri file-path ))))

(defun eglot-java-run-test ()
  "Run a test class."
  (interactive)
  (let* ((fqcn                 (eglot-java--class-fqcn))
         (cp                   (eglot-java--project-classpath (buffer-file-name) "test"))
         (current-file-is-test (not (equal ':json-false (eglot-java--file--test-p (buffer-file-name))))))

    (unless (file-exists-p +java/junit-platform-console-standalone-jar)
      (user-error "%s doest not exit" +java/junit-platform-console-standalone-jar))

    (if current-file-is-test
        (compile
         (concat "java -jar "
                 +java/junit-platform-console-standalone-jar
                 (if (string-match-p "#" fqcn)
                     " -m "
                   " -c ")
                 fqcn
                 " -class-path "
                 (mapconcat #'identity cp path-separator)
                 " ")
         t)
      (user-error "No test found in current file! Is the file saved?" ))))

(defun eglot-java-run-main ()
  "Run a main class."
  (interactive)
  (let* ((fqcn (eglot-java--class-fqcn t))
         (cp   (eglot-java--project-classpath (buffer-file-name) "runtime")))
    (if fqcn
        (compile
         (concat "java -cp "
                 (mapconcat #'identity cp path-separator)
                 " "
                 fqcn)
         t)
      (user-error "No main method found in this file! Is the file saved?!"))))

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

;; POM.xml format
(add-hook-run-once
 'pom-xml-mode-hook
 (lambda nil
   (setq +java/pom-formatter-buffer-name "*format pom xml*")

   (add-to-list 'display-buffer-alist
                `(,+java/pom-formatter-buffer-name display-buffer-below-selected))

   (defun +java/sortpom-formatter ()
     (interactive)
     (let ((output-buffer (get-buffer-create +java/pom-formatter-buffer-name))
           (cmd (s-join " " '("mvn"
                              "com.github.ekryd.sortpom:sortpom-maven-plugin:sort"
                              ;; "-Dsort.nrOfIndentSpace=4"
                              "-Dsort.keepBlankLines"
                              "-Dsort.predefinedSortOrder=custom_1"
                              "-Dsort.createBackupFile=false"))))
       (async-shell-command cmd output-buffer)))

   (defun +java/tidy:check ()
     (interactive)
     (let ((output-buffer (get-buffer-create +java/pom-formatter-buffer-name))
           (cmd "mvn tidy:check"))
       (async-shell-command cmd output-buffer)))

   (defun +java/tidy:pom ()
     (interactive)
     (let ((output-buffer (get-buffer-create +java/pom-formatter-buffer-name))
           (cmd "mvn tidy:pom"))
       (async-shell-command cmd output-buffer)))

   ;; NOTE: xml-format-maven-plugin
   ;; https://acegi.github.io/xml-format-maven-plugin/usage.html

   (+funcs/major-mode-leader-keys
    pom-xml-mode-map
    "c" '(+java/tidy:check :which-key "tidy:check")
    "f" '(+java/tidy:pom :which-key "tidy:pom")
    "F" '(+java/sortpom-formatter :which-key "sortpom:sort"))))


(provide 'init-lang-java)

;;; init-lang-java.el ends here

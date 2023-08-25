;; init-lang-java.el --- Java Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Java Configurations
;;

;;; Code:

;; NOTE: Install `jdtls', https://github.com/eclipse/eclipse.jdt.ls
;; NOTE: Install decomiplers, https://github.com/dgileadi/dg.jdt.ls.decompiler, https://marketplace.visualstudio.com/items?itemName=dgileadi.java-decompiler
;; NOTE: `jdtls' settings: https://github.com/eclipse/eclipse.jdt.ls/blob/master/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/preferences/Preferences.java
;; NOTE: Formatter settings:
;; Eclipse formatter settings: https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings
;; continuation_indentation: https://stackoverflow.com/questions/42622553/eclipse-code-formatter-indents-with-double-amount-of-spaces-intellij-ide
(setq java-ts-mode-indent-offset 2)

(add-hook-run-once 'java-mode-hook #'+eglot/set-leader-keys)
(add-hook-run-once 'java-ts-mode-hook #'+eglot/set-leader-keys)

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs)

  ;; ----------------------- Intialization/Configurations -----------------------
  (defun jdtls-command-contact (&optional interactive)
    (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "jdtls-cache"))
           (project-dir (file-name-nondirectory (directory-file-name (+project/root))))
           (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
           (jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.26/lombok-1.18.26.jar"))
                       "-XX:+UseZGC"
                       "-XX:+UseStringDeduplication"))
           (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
           (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
      contact))

  ;; TODO: dynamic jdk path
  (defun jdtls-initialization-options ()
    (let* ((ostype (cond
                    (IS-WSL "WSL")
                    (IS-LINUX "Linux")
                    (IS-MAC "Darwin")
                    (t "")))
           (setting-json-file (file-name-concat user-emacs-directory "lsp-config" (format "jdtls-%s.json" ostype))))
      (with-temp-buffer
        (insert-file-contents setting-json-file)
        (json-parse-buffer :object-type 'plist :false-object :json-false))))

  (cl-defmethod eglot-initialization-options (server &context (major-mode java-mode))
    (jdtls-initialization-options))

  (cl-defmethod eglot-initialization-options (server &context (major-mode java-ts-mode))
    (jdtls-initialization-options))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode java-mode))
    (plist-get (jdtls-initialization-options) :settings))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode java-ts-mode))
    (plist-get (jdtls-initialization-options) :settings))

  ;; ----------------------- Support URI jdt:// protocol -----------------------
  (defun +eglot/jdtls-uri-to-path (uri)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (when-let* ((jdt-scheme-p (string-prefix-p "jdt://" uri))
                (filename (when (string-match "^jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                            (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
                (source-dir (file-name-concat (project-root (eglot--current-project)) ".eglot"))
                (source-file (expand-file-name (file-name-concat source-dir filename))))
      (unless (file-directory-p source-dir)
        (make-directory source-dir t))
      (unless (file-readable-p source-file)
        (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                        :java/classFileContents
                                        (list :uri uri))))
          (with-temp-file source-file (insert content))))
      (puthash source-file uri eglot-path-uri-cache)
      source-file))

  (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-mode))
    (+eglot/jdtls-uri-to-path uri))

  (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-ts-mode))
    (+eglot/jdtls-uri-to-path uri))

  ;; ----------------------- Support jdt.ls extra commands -----------------------
  ;; (defun java-apply-workspaceEdit (arguments)
  ;;   "Command `java.apply.workspaceEdit' handler."
  ;;   (mapc #'eglot--apply-workspace-edit arguments))

  (defun java-action-overrideMethodsPrompt (arguments)
    "Command `java.action.overrideMethodsPrompt' handler."
    (let* ((argument (aref arguments 0))
           (list-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                                 :java/listOverridableMethods
                                                 argument))
           (methods (plist-get list-methods-result :methods))
           (menu-items (mapcar (lambda (method)
                                 (let* ((name (plist-get method :name))
                                        (parameters (plist-get method :parameters))
                                        (class (plist-get method :declaringClass)))
                                   (cons (format "%s(%s) class: %s" name (string-join parameters ", ") class) method)))
                               methods))
           (selected-methods (cl-map 'vector
                                     (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                     (delete-dups
                                      (completing-read-multiple "overridable methods: " menu-items))))
           (add-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                                :java/addOverridableMethods
                                                (list :overridableMethods selected-methods :context argument))))
      (eglot--apply-workspace-edit add-methods-result)))

  (defun +java/execute-command (server _command)
    (eglot--dbind ((Command) command arguments) _command
      (pcase command
        ;; ("java.apply.workspaceEdit" (java-apply-workspaceEdit arguments))
        ("java.action.overrideMethodsPrompt" (java-action-overrideMethodsPrompt arguments))
        (_ (eglot--request server :workspace/executeCommand _command)))))

  (defun +java/eglot-execute (server action)
    "Ask SERVER to execute ACTION.
ACTION is an LSP object of either `CodeAction' or `Command' type."
    (eglot--dcase action
      (((Command)) (+java/execute-command server action))
      (((CodeAction) edit command data)
       (if (and (null edit) (null command) data
                (eglot--server-capable :codeActionProvider :resolveProvider))
           (eglot-execute server (eglot--request server :codeAction/resolve action))
         (when edit (eglot--apply-workspace-edit edit))
         (when command (+java/execute-command server command))))))

  (cl-defmethod eglot-execute (server action &context (major-mode java-mode))
    (+java/eglot-execute server action))

  (cl-defmethod eglot-execute (server action &context (major-mode java-ts-mode))
    (+java/eglot-execute server action)))

;; Run junit console
(with-eval-after-load 'java-ts-mode

  ;; Download `junit-platform-console-standalone.jar'
  (defvar +java/junit-platform-console-standalone-jar
    (expand-file-name (locate-user-emacs-file "cache/lsp-servers/java/junit-console/junit-platform-console-standalone.jar")))

  (+funcs/major-mode-leader-keys
   java-ts-mode-map
   "r" '(+java/junit-console-run-dwim :which-key "junit-console-run-dwim"))

  ;; check junit console launcher options for details
  (defun +java/junit-console-run-dwim ()
    "Java run main/test at point."
    (interactive)
    (let* ((pkg (+java/treesit-get-package))
           (class (+java/treesit-get-class))
           (method (+java/treesit-get-method))
           (classpath (+java/eglot-get-project-classpath)))
      (if (and pkg class classpath)
          (compile
           (concat "java -jar " +java/junit-platform-console-standalone-jar
                   " -cp " classpath
                   (if method
                       (format " -m '%s.%s#%s'" pkg class method)
                     (format " -c '%s.%s'" pkg class)))
           t)
        (message "Can not found package/class/classpath"))))

  (defun +java/treesit-get-package-node ()
    (treesit-node-text
     (car
      (treesit-filter-child
       (treesit-buffer-root-node)
       (lambda (child)
         (member (treesit-node-type child) '("package_declaration")))))
     t))

  (defun +java/treesit-get-package ()
    (let ((p (+java/treesit-get-package-node)))
      (when (string-match "package \\(.+\\);" p)
        (match-string 1 p))))

  (defun +java/treesit-get-class ()
    (treesit-defun-name
     (car
      (treesit-filter-child
       (treesit-buffer-root-node)
       (lambda (child)
         (member (treesit-node-type child) '("class_declaration")))))))

  (defun +java/treesit-get-method ()
    (treesit-defun-name
     (treesit-parent-until
      (treesit-node-at (point))
      (lambda (parent)
        (member (treesit-node-type parent) '("method_declaration"))))))

  (defun +java/maven-get-deps-classpath (target-location)
    "Get dependencies classpath."
    (let* ((project-root-path (+project/root))
           (default-directory project-root-path)
           (deps-cp-file (format "%s/deps-cp" target-location)))
      (unless (file-exists-p deps-cp-file)
        ;; NOTE: Cache deps classpath to speed up shell command, regenerate it once you modify project dependencies.
        (shell-command-to-string "mvn test-compile dependency:build-classpath -Dmdep.includeScope=test -Dmdep.outputFile=target/deps-cp"))
      (with-temp-buffer
        (insert-file-contents deps-cp-file)
        (buffer-string))))

  (defun +java/maven-get-project-classpath ()
    (when-let* ((target-location (expand-file-name (locate-dominating-file default-directory "target")))
                (target-path (format "%starget" target-location))
                (deps-cp (+java/maven-get-deps-classpath target-path)))
      (format "%s/classes:%s/test-classes:%s" target-path target-path deps-cp)))

  (defun +java/eglot-get-project-classpath (&optional filename scope)
    (let* ((filename (or filename (buffer-file-name)))
           (scope (or scope "test"))
           (command (list
                     :title ""
                     :command "java.project.getClasspaths"
                     :arguments (vector (eglot--path-to-uri filename)
                                        (json-serialize (list :scope scope)))))
           (classpaths (plist-get (eglot-execute (eglot--current-server-or-lose) command) :classpaths)))
      (mapconcat #'identity classpaths path-separator)))

  (defun +java/testfile-p (file-path)
    "Tell if a file locate at FILE-PATH is a test class."
    (let ((command (list
                    :title ""
                    :command "java.project.isTestFile"
                    :arguments (vector (eglot--path-to-uri file-path)))))
      (eq t (eglot-execute (eglot--current-server-or-lose) command)))))

;; http://www.tianxiangxiong.com/2017/02/12/decompiling-java-classfiles-in-emacs.html
;; https://github.com/xiongtx/jdecomp
;; https://github.com/JetBrains/intellij-community/tree/master/plugins/java-decompiler/engine
;; java -cp /home/zsxh/.local/share/JetBrains/Toolbox/apps/IDEA-C/ch-0/203.7148.57/plugins/java-decompiler/lib/java-decompiler.jar org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler [-<option>=<value>]* [<source>]+ <destination>
;; TODO: `jdecomp--fernflower-decompile-file' should extract all A.class and A${anonymous}.class
(use-package jdecomp
  :commands (jdecomp-mode)
  :config
  (setq jdecomp-decompiler-type 'fernflower
        jdecomp-decompiler-paths `((fernflower . ,(file-name-concat user-emacs-directory "cache" "lsp-servers" "java" "bundles" "dg.jdt.ls.decompiler.fernflower-0.0.3.jar")))
        jdecomp-decompiler-options '((fernflower "-hes=0" "-hdc=0" "-fdi=0"))))


(provide 'init-lang-java)

;;; init-lang-java.el ends here

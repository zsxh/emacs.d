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

;; NOTE: jdtls.py script add: os.environ["JAVA_HOME"] = subprocess.check_output(["mise", "where", "java@21"]).decode("utf-8").rstrip()
(defun jdtls-command-contact (&optional interactive project)
  (let* ((jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.30/lombok-1.18.30.jar"))
                     "-XX:+UseZGC"
                     "-XX:+ZGenerational"
                     "-XX:+UseStringDeduplication"))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         (contact (append '("jdtls") jvm-args)))
    contact))

(with-eval-after-load 'eglot
  ;; ----------------------- Intialization/Configurations -----------------------
  ;; (jsonrpc--json-encode (jdtls-initialization-options))
  (defun jdtls-initialization-options ()
    `(:settings (:java (:autobuild (:enabled t)
                        :configuration (:runtimes [(:name "JavaSE-1.8"
                                                    :path ,(string-trim (shell-command-to-string "mise where java@8")))
                                                   (:name "JavaSE-11"
                                                    :path ,(string-trim (shell-command-to-string "mise where java@11")))
                                                   (:name "JavaSE-17"
                                                    :path ,(string-trim (shell-command-to-string "mise where java@17")))
                                                   (:name "JavaSE-21"
                                                    :path ,(string-trim (shell-command-to-string "mise where java@21"))
                                                    :default t)])
                        :format (:settings (:url ,(expand-file-name (locate-user-emacs-file "cache/eclipse-java-google-style.xml"))
                                            :profile "GoogleStyle"))
                        :completion (:guessMethodArguments t
                                     :lazyResolveTextEdit (:enabled t)
                                     :favoriteStaticMembers ["org.junit.Assert.*"
                                                             "org.junit.Assume.*"
                                                             "org.junit.jupiter.api.Assertions.*"
                                                             "org.junit.jupiter.api.Assumptions.*"
                                                             "org.junit.jupiter.api.DynamicContainer.*"
                                                             "org.junit.jupiter.api.DynamicTest.*"
                                                             "org.mockito.Mockito.*"
                                                             "org.mockito.ArgumentMatchers.*"
                                                             "org.mockito.Answers.*"])
                        :edit (:validateAllOpenBuffersOnChanges :json-false)
                        ;; Javadoc generation, https://github.com/mfussenegger/nvim-jdtls/issues/76#issuecomment-831448277
                        :codeGeneration (:generateComments t)))
      :extendedClientCapabilities (:classFileContentsSupport t
                                   :overrideMethodsPromptSupport t)
      :bundles ,(if-let* ((bundles-dir (file-name-concat user-emacs-directory "cache" "lsp-servers" "java" "bundles"))
                          (_ (file-directory-p bundles-dir))
                          (jars (directory-files bundles-dir t "\\.jar$")))
                    (apply #'vector jars)
                  [])))

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
  ;;   (mapc #'eglot--apply-workspace-edit arguments this-command))

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
           ;; use ";" instead of "," to separate strings in completing-read-multiple
           (crm-separator "[ \t]*;[ \t]*")
           (selected-methods (cl-map 'vector
                                     (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                     (delete-dups
                                      (completing-read-multiple "overridable methods: " menu-items))))
           (add-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                                :java/addOverridableMethods
                                                (list :overridableMethods selected-methods :context argument))))
      (eglot--apply-workspace-edit add-methods-result this-command)))

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
                (eglot-server-capable :codeActionProvider :resolveProvider))
           (eglot-execute server (eglot--request server :codeAction/resolve action))
         (when edit (eglot--apply-workspace-edit edit this-command))
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
                     :arguments (vector (eglot-path-to-uri filename)
                                        (json-serialize (list :scope scope)))))
           (classpaths (plist-get (eglot-execute (eglot--current-server-or-lose) command) :classpaths)))
      (mapconcat #'identity classpaths path-separator)))

  (defun +java/testfile-p (file-path)
    "Tell if a file locate at FILE-PATH is a test class."
    (let ((command (list
                    :title ""
                    :command "java.project.isTestFile"
                    :arguments (vector (eglot-path-to-uri file-path)))))
      (eq t (eglot-execute (eglot--current-server-or-lose) command)))))

;; ==================== Viewing Java Class Files in Emacs ====================
;;
;; https://nullprogram.com/blog/2012/08/01/
;;
(defun javap-handler (operation &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq operation 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension
                       (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        ;; (java-mode)
        (current-buffer))))
   ;; Run the real handler without the javap handler installed
   (t (let ((inhibit-file-name-handlers
             (cons 'javap-handler
                   (and (eq inhibit-file-name-operation operation)
                        inhibit-file-name-handlers)))
            (inhibit-file-name-operation operation))
        (apply operation args)))))

(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))


(provide 'init-lang-java)

;;; init-lang-java.el ends here

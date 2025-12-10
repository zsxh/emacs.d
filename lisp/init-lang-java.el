;; init-lang-java.el --- Java Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Java Configurations
;;

;;; Code:

;; NOTE: Install `jdtls' via Nix home-manager/darwin-nix, https://github.com/eclipse/eclipse.jdt.ls
;; NOTE: `jdtls' settings: https://github.com/eclipse/eclipse.jdt.ls/blob/master/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/preferences/Preferences.java
;; NOTE: Formatter settings:
;; Eclipse formatter settings: https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings
;; continuation_indentation: https://stackoverflow.com/questions/42622553/eclipse-code-formatter-indents-with-double-amount-of-spaces-intellij-ide
;; (setq java-ts-mode-indent-offset 2)

(when (and (version< emacs-version "31")
           (treesit-ready-p 'java))
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode)))

(add-hook-run-once 'java-mode-hook #'+eglot/set-leader-keys)
(add-hook-run-once 'java-ts-mode-hook #'+eglot/set-leader-keys)

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-ts-mode-hook #'eglot-ensure)

(defun jdtls-command-contact (&optional interactive project)
  (let* ((jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.38/lombok-1.18.38.jar"))
                     ;; "-XX:+UseZGC"
                     ;; "-XX:+ZGenerational"
                     "-XX:+UseStringDeduplication"))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         (contact (append '("jdtls") jvm-args)))
    contact))

;; TODO: eglot does not support `workspace.workspaceEdit.resourceOperations' yet
(with-eval-after-load 'eglot
  (exec-path-from-shell-copy-envs
   '("JAVA_8_HOME" "JAVA_11_HOME" "JAVA_17_HOME"
     "JAVA_21_HOME" "JAVA_23_HOME" "JAVA_25_HOME"))

  ;; ----------------------- Intialization/Configurations -----------------------
  ;; TODO: install jdtls bundles/plugins from `mason'
  (defun eglot-java-bundles ()
    "Return a vector of JAR files from the jdtls bundles directory."
    (if-let* ((bundles-dir (file-name-concat user-emacs-directory "cache" "lsp-servers" "java" "bundles"))
              (_ (file-directory-p bundles-dir))
              (jars (directory-files bundles-dir t "\\.jar$")))
        (apply #'vector jars)
      []))

  (defvar eglot-java-workspace-configuration
    `(:java
      (:autobuild (:enabled t)
       :configuration (:runtimes [(:name "JavaSE-1.8"
                                   :path ,(getenv "JAVA_8_HOME"))
                                  ;; (:name "JavaSE-11"
                                  ;;  :path ,(getenv "JAVA_11_HOME"))
                                  ;; (:name "JavaSE-17"
                                  ;;  :path ,(getenv "JAVA_17_HOME"))
                                  ;; (:name "JavaSE-21"
                                  ;;  :path ,(getenv "JAVA_21_HOME"))
                                  (:name "JavaSE-25"
                                   :path ,(getenv "JAVA_25_HOME")
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
       :codeGeneration (:generateComments t) ;; https://github.com/mfussenegger/nvim-jdtls/issues/76#issuecomment-831448277
       :referencesCodeLens (:enabled :json-false) ;; https://github.com/redhat-developer/vscode-java/issues/148
       :implementationCodeLens "none" ;; one of [none, types, methods, all]
       )))

  (cl-defmethod eglot-initialization-options (server &context (major-mode java-mode))
    `(:settings ,eglot-java-workspace-configuration
      :extendedClientCapabilities (:classFileContentsSupport t
                                   :overrideMethodsPromptSupport t
                                   :hashCodeEqualsPromptSupport t
                                   :executeClientCommandSupport t
                                   :advancedOrganizeImportsSupport t ; require `executeClientCommandSupport'
                                   :generateConstructorsPromptSupport t
                                   :generateToStringPromptSupport t
                                   :advancedGenerateAccessorsSupport t
                                   ;; :advancedExtractRefactoringSupport t
                                   ;; :moveRefactoringSupport t
                                   ;; :resolveAdditionalTextEditsSupport t
                                   )
      :bundles ,(eglot-java-bundles)))

  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode java-mode))
    eglot-java-workspace-configuration)

  ;; ----------------------- Support URI jdt:// protocol -----------------------
  (defun +java/eglot-find-jdt-server ()
    (let ((filter-fn (lambda (server)
                       (cl-loop for (mode . languageid) in
                                (eglot--languages server)
                                when (string= languageid "java")
                                return languageid)))
          (servers (gethash (eglot--current-project) eglot--servers-by-project)))
      (cl-find-if filter-fn servers)))

  (defun +java/eglot-jdt-uri-handler (operation &rest args)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (let* ((uri (car args))
           (cache-dir (expand-file-name "eglot-java" (temporary-file-directory)))
           (_ (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri))
           (jar-file (substring uri (match-beginning 1) (match-end 1)))
           (java-file (format "%s.java" (replace-regexp-in-string "/" "." (substring uri (match-beginning 2) (match-end 2)) t t)))
           (jar-dir (concat (file-name-as-directory cache-dir)
                            (file-name-as-directory jar-file)))
           (source-file (expand-file-name (concat jar-dir java-file))))
      (unless (file-readable-p source-file)
        (let ((content (eglot--request
                        (or (eglot-current-server)
                            ;; NOTE: dape https://github.com/svaante/dape/issues/78#issuecomment-1966786597
                            (+java/eglot-find-jdt-server))
                        :java/classFileContents (list :uri uri))))
          (unless (file-directory-p jar-dir) (make-directory jar-dir t))
          (with-temp-file source-file (insert content))))
      (cond
       ((eq operation 'expand-file-name) source-file)
       ((eq operation 'file-truename) source-file)
       ((eq operation 'file-local-name) source-file)
       ((eq operation 'file-remote-p) nil)
       ;; Handle any operation we don’t know about.
       (t (let ((inhibit-file-name-handlers
                 (cons '+java/eglot-jdt-uri-handler
                       (and (eq inhibit-file-name-operation operation)
                            inhibit-file-name-handlers)))
                (inhibit-file-name-operation operation))
            (apply operation args))))))

  (add-to-list 'file-name-handler-alist '("\\`jdt://" . +java/eglot-jdt-uri-handler))

  ;; ----------------------- Support jdt.ls extra commands -----------------------
  (defun java-apply-workspaceEdit (arguments)
    "Command `java.apply.workspaceEdit' handler."
    (mapc #'eglot--apply-workspace-edit arguments this-command))

  (defun java-action-overrideMethodsPrompt (server arguments)
    "Command `java.action.overrideMethodsPrompt' handler."
    (let* ((argument (seq-elt arguments 0))
           (list-methods-result (eglot--request server :java/listOverridableMethods argument))
           (methods (plist-get list-methods-result :methods))
           (menu-items (mapcar
                        (lambda (method)
                          (let* ((name (plist-get method :name))
                                 (parameters (plist-get method :parameters))
                                 (class (plist-get method :declaringClass)))
                            (cons (format "%s(%s): %s" name (string-join parameters ", ") class) method)))
                        methods))
           ;; use ";" instead of "," to separate strings in completing-read-multiple
           (crm-separator "[ \t]*;[ \t]*")
           (selected-methods (cl-map
                              'vector
                              (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                              (delete-dups
                               (completing-read-multiple "Select methods: " menu-items))))
           (add-methods-result (eglot--request
                                server
                                :java/addOverridableMethods
                                (list :overridableMethods selected-methods :context argument))))
      (eglot--apply-workspace-edit add-methods-result this-command)))

  (defun java-show-references (command arguments)
    "Show Java references from LSP arguments."
    (if-let* ((refs (seq-elt arguments 2))
              (_ (length> refs 0)))
        (xref-show-xrefs
         (eglot--collecting-xrefs (collect)
           (mapc
            (lambda (ref)
              (eglot--dbind ((Location) uri range) ref
                (collect (eglot--xref-make-match "" uri range))))
            refs))
         nil)
      (message "%s returned no references" command)))

  (defun java-action-rename (arguments)
    "Execute Java rename action using Eglot LSP."
    (eglot--dbind (uri offset length) arguments
      (with-current-buffer (find-file (eglot-uri-to-path uri))
        (deactivate-mark)
        (goto-char (1+ offset))
        (set-mark (point))
        (goto-char (+ (point) length))
        (exchange-point-and-mark)
        (sit-for 0.5)
        (call-interactively 'eglot-rename)
        (deactivate-mark))))

  (defun java-action-generateToStringPrompt (server arguments)
    "Prompt user to generate toString method for Java class using Eglot LSP."
    (let* ((context (seq-elt arguments 0))
           (check-resp (eglot--request server :java/checkToStringStatus context)))
      (eglot--dbind (fields exists) check-resp
        (when (or (eq exists :json-false) (y-or-n-p "The toString() method already exists. Replace?"))
          (let* ((menu-items (mapcar (lambda (field)
                                       (let ((name (plist-get field :name))
                                             (type (plist-get field :type)))
                                         (cons (format "%s: %s" name type) field)))
                                     fields))
                 ;; use ";" instead of "," to separate strings in completing-read-multiple
                 (crm-separator "[ \t]*;[ \t]*")
                 (selected-items (cl-map 'vector
                                         (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                         (delete-dups
                                          (completing-read-multiple "Select fields to include: " menu-items))))
                 (generate-result (eglot--request server :java/generateToString (list :fields selected-items :context context))))
            (eglot--apply-workspace-edit generate-result this-command))))))

  (defun java-action-hashCodeEqualsPrompt (server arguments)
    "Prompt user to generate hashCode and equals methods for Java class using Eglot LSP."
    (let* ((context (seq-elt arguments 0))
           (check-resp (eglot--request server :java/checkHashCodeEqualsStatus context)))
      (eglot--dbind (fields existingMethods) check-resp
        (when (or (seq-empty-p existingMethods)
                  (y-or-n-p (format "The %s method already exists. Replace?" existingMethods)))
          (let* ((menu-items (mapcar (lambda (field)
                                       (let ((name (plist-get field :name))
                                             (type (plist-get field :type)))
                                         (cons (format "%s: %s" name type) field)))
                                     fields))
                 ;; use ";" instead of "," to separate strings in completing-read-multiple
                 (crm-separator "[ \t]*;[ \t]*")
                 (selected-items (cl-map 'vector
                                         (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                         (delete-dups
                                          (completing-read-multiple "Select fields to include: " menu-items))))
                 (generate-result (eglot--request server :java/generateHashCodeEquals
                                                  (list
                                                   :fields selected-items
                                                   :context context
                                                   :regenerate (not (seq-empty-p existingMethods))))))
            (eglot--apply-workspace-edit generate-result this-command))))))

  (cl-defmethod eglot-handle-request
    (_server (_method (eql workspace/executeClientCommand))
             &key command arguments &allow-other-keys)
    (pcase command
      ("java.action.organizeImports.chooseImports"
       (setq t1 command)
       (setq t2 arguments)
       (let* ((documentUri (seq-elt arguments 0)) ; string - 文档 URI
              (selections (seq-elt arguments 1)) ; ImportSelection[] - 导入冲突列表
              (restoreExistingImports (seq-elt arguments 2)) ; boolean - 是否保留现有导入
              (select-candidate-fn
               (eglot--lambda (candidates range)
                 (eglot--goto range)
                 (let* ((menu-items (mapcar
                                     (lambda (cand)
                                       (let ((fullyQualifiedName (plist-get cand :fullyQualifiedName)))
                                         (cons fullyQualifiedName cand)))
                                     candidates))
                        (selected-item-key (completing-read "Select class to import: " menu-items))
                        (selected-item (alist-get selected-item-key menu-items nil nil 'equal)))
                   selected-item))))
         (with-current-buffer (find-file (eglot-uri-to-path documentUri))
           (save-excursion
             (cl-map 'vector select-candidate-fn selections)))))
      ;; ("_java.reloadBundles.command" (eglot-java-bundles))
      ("_java.reloadBundles.command" [])
      (_ (message "Unknown client command: %s" command))))

  (defun java-action-organizeImports ()
    "Organize imports in the current Java buffer using Eglot LSP."
    (interactive)
    (eglot--async-request
     (eglot--current-server-or-lose)
     :java/organizeImports
     `(:textDocument (:uri ,(eglot-path-to-uri (buffer-file-name) :truenamep t))
       :range (:start (:line 0 :character 0)
               :end (:line 0 :character 0))
       :context (:diagnostics []))
     :success-fn (lambda (result)
                   (eglot--apply-workspace-edit result this-command))
     :hint :java/organizeImports))

  (defun java-action-generateAccessorsPrompt (server arguments)
    "Prompt user to generate accessor methods for Java fields using Eglot LSP."
    (let* ((context (seq-elt arguments 0))
           (accessorFields (eglot--request server :java/resolveUnimplementedAccessors context))
           (menu-items (mapcar (lambda (accessorField)
                                 (let ((field (plist-get accessorField :fieldName))
                                       (type (plist-get accessorField :typeName)))
                                   (cons (format "%s: %s" field type) accessorField)))
                               accessorFields))
           ;; use ";" instead of "," to separate strings in completing-read-multiple
           (crm-separator "[ \t]*;[ \t]*")
           (selected-items (cl-map 'vector
                                   (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                   (delete-dups
                                    (completing-read-multiple "Select fields to generate: " menu-items))))
           (generate-result (eglot--request server :java/generateAccessors (list :accessors selected-items :context context))))
      (eglot--apply-workspace-edit generate-result this-command)))

  (defun java-action-generateConstructorsPrompt (server arguments)
    "Prompt user to generate constructors for Java classes using Eglot LSP."
    (let* ((context (seq-elt arguments 0))
           (check-resp (eglot--request server :java/checkConstructorsStatus context))
           (constructors (plist-get check-resp :constructors))
           (fields (plist-get check-resp :fields))
           ;; use ";" instead of "," to separate strings in completing-read-multiple
           (crm-separator "[ \t]*;[ \t]*")
           (constructor-items (mapcar (lambda (item)
                                        (let ((name (plist-get item :name))
                                              (parameters (plist-get item :parameters)))
                                          (cons (format "%s(%s)" name (mapconcat #'identity parameters ", ")) item)))
                                      constructors))
           (selected-constructors (cl-map 'vector
                                          (lambda (choice) (alist-get choice constructor-items nil nil 'equal))
                                          (delete-dups
                                           (completing-read-multiple "Select constructors to generate: " constructor-items))))
           (field-items (mapcar (lambda (item)
                                  (let ((name (plist-get item :name))
                                        (type (plist-get item :type)))
                                    (cons (format "%s: %s" name type) item)))
                                fields))
           (selected-fields (cl-map 'vector
                                    (lambda (choice) (alist-get choice field-items nil nil 'equal))
                                    (delete-dups
                                     (completing-read-multiple "Select fields to generate: " field-items))))
           (generate-result (eglot--request server :java/generateConstructors
                                            (list :context context
                                                  :constructors selected-constructors
                                                  :fields selected-fields))))
      (eglot--apply-workspace-edit generate-result this-command)))

  (cl-defmethod eglot-execute :around
    (server action &context (major-mode java-mode))
    "Custom handler for performing client commands."
    (let ((command (plist-get action :command))
          (arguments (plist-get action :arguments)))
      (pcase command
        ("java.apply.workspaceEdit" (java-apply-workspaceEdit arguments))
        ("java.action.overrideMethodsPrompt" (java-action-overrideMethodsPrompt server arguments))
        ("java.action.generateToStringPrompt" (java-action-generateToStringPrompt server arguments))
        ("java.action.hashCodeEqualsPrompt" (java-action-hashCodeEqualsPrompt server arguments))
        ("java.action.generateAccessorsPrompt" (java-action-generateAccessorsPrompt server arguments))
        ("java.action.generateConstructorsPrompt" (java-action-generateConstructorsPrompt server arguments))
        ("java.action.applyRefactoringCommand" (message "Unhandled method %s" command))
        ("java.action.rename" (java-action-rename arguments))
        ("java.show.references" (java-show-references command arguments))
        ("java.show.implementations" (java-show-references command arguments))
        (_ (cl-call-next-method)))))
  )

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

(add-hook 'emacs-startup-hook
          (lambda ()
            (add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))))


(provide 'init-lang-java)

;;; init-lang-java.el ends here

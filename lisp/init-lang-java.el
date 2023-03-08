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

;; NOTE: Install `jdtls', https://github.com/eclipse/eclipse.jdt.ls
;; NOTE: Install decomiplers, https://github.com/dgileadi/dg.jdt.ls.decompiler, https://marketplace.visualstudio.com/items?itemName=dgileadi.java-decompiler

;; NOTE: `jdtls' settings: https://github.com/eclipse/eclipse.jdt.ls/blob/master/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/preferences/Preferences.java

(add-hook-run-once 'java-mode-hook #'+eglot/set-leader-keys)
(add-hook-run-once 'java-ts-mode-hook #'+eglot/set-leader-keys)

(add-hook 'java-mode-hook #'eglot-ensure)
(add-hook 'java-ts-mode-hook #'eglot-ensure)

(with-eval-after-load 'eglot
  (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs)

  (defun jdtls-command-contact (&optional interactive)
    (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
           (project-dir (file-name-nondirectory (directory-file-name (+project/root))))
           (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
           (jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))
                       "-Xmx8G"
                       ;; "-XX:+UseG1GC"
                       "-XX:+UseZGC"
                       "-XX:+UseStringDeduplication"
                       ;; "-XX:FreqInlineSize=325"
                       ;; "-XX:MaxInlineLevel=9"
                       "-XX:+UseCompressedOops"))
           (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
           (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
      contact))

  (defun jdtls-initialization-options ()
    (let ((setting-json-file (file-name-concat user-emacs-directory "lsp-config" "jdtls.json")))
      (with-temp-buffer
        (insert-file-contents setting-json-file)
        (json-parse-buffer :object-type 'plist))))

  (cl-defmethod eglot-initialization-options (server &context (major-mode java-mode))
    (jdtls-initialization-options))

  (cl-defmethod eglot-initialization-options (server &context (major-mode java-ts-mode))
    (jdtls-initialization-options))

  ;; https://github.com/joaotavora/eglot/discussions/888#discussioncomment-2386710
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments))

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
      (setq eglot-path-uri-cache (plist-put eglot-path-uri-cache (intern source-file) uri))
      source-file))

  (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-mode))
    (+eglot/jdtls-uri-to-path uri))

  (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-ts-mode))
    (+eglot/jdtls-uri-to-path uri)))


;; Run junit console
(with-eval-after-load 'java-ts-mode

  ;; Download http://repository.sonatype.org/service/local/artifact/maven/redirect?r=central-proxy&g=org.junit.platform&a=junit-platform-console-standalone&v=LATEST
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
           (target-location (expand-file-name (locate-dominating-file default-directory "target")))
           (target-path (format "%starget" target-location))
           (deps-cp (+java/get-deps-classpath target-path))
           (class-path (format "%s/classes:%s/test-classes:%s" target-path target-path deps-cp)))
      (if (and pkg class target-location)
          (compile
           (concat "java -jar " +java/junit-platform-console-standalone-jar
                   " -cp " class-path
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

  (defun +java/get-deps-classpath (target-location)
    "Get dependencies classpath."
    (let* ((project-root-path (+project/root))
           (default-directory project-root-path)
           (deps-cp-file (format "%s/deps-cp" target-location)))
      (unless (file-exists-p deps-cp-file)
        ;; NOTE: Cache deps classpath to speed up shell command, regenerate it once you modify project dependencies.
        (shell-command-to-string "mvn test-compile dependency:build-classpath -Dmdep.includeScope=test -Dmdep.outputFile=target/deps-cp"))
      (with-temp-buffer
        (insert-file-contents deps-cp-file)
        (buffer-string)))))

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

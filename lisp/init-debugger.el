;; init-debugger.el --- Debugger Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Debugger Settings
;;

;;; Code:

;; [Dape] Debug Adapter Protocol for Emacs
;; TODO: got to libaries source
;; NOTE: https://github.com/svaante/dape#supported-debug-adapters
;; NOTE: https://microsoft.github.io/debug-adapter-protocol/overview
(use-package dape
  :vc (:url "https://github.com/svaante/dape" :rev :newest)
  :after eglot
  :config
  (setq dape-inline-variables t))

(with-eval-after-load 'dape
  ;; Python
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-ts-mode python-mode)
                 command "python3"
                 command-args ("-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default))

  ;; Java
  ;; TODO: debug test class/method
  ;; install vscode-java-debug, https://github.com/Microsoft/vscode-java-debug
  ;; install vscode-java-test, https://github.com/microsoft/vscode-java-test
  ;;
  ;; https://github.com/microsoft/java-debug#usage-with-eclipsejdtls
  ;; To use java-debug as a jdt.ls plugin, an LSP client has to launch jdt.ls with initializationOptions
  ;; that contain the path to the built java-debug jar within a bundles array
  ;; Once eclipse.jdt.ls launched, the client can send a Command(`vscode.java.startDebugSession')
  ;; to the server to start a debug session and get port number
  (defun dape-java ()
    (interactive)
    (let* ((server (eglot--current-server-or-lose))
           (port (eglot-execute server '(:command "vscode.java.startDebugSession" :title "" :arguments [])))
           (mainclass-items (mapcar (lambda (item)
                                      (let ((mainclass (plist-get item :mainClass))
                                            (projectname (plist-get item :projectName)))
                                        (cons (format "mainClass:[%s] projectName:[%s]" mainclass projectname) item)))
                                    (eglot-execute server '(:command "vscode.java.resolveMainClass" :title "" :arguments []))))
           (mainclass-item (alist-get (completing-read "Main Class:" mainclass-items) mainclass-items nil nil 'equal))
           (mainclass (plist-get mainclass-item :mainClass))
           (projectname (plist-get mainclass-item :projectName))
           (classpath (aref (eglot-execute server `(:command "vscode.java.resolveClasspath" :title "" :arguments [,mainclass ,projectname])) 1)))
      (add-to-list 'dape-configs
                   `(java
                     modes (java-ts-mode java-mode)
                     host "localhost"
                     port ,port
                     :type "java"
                     :request "launch"
                     :cwd dape-cwd-fn
                     :mainClass ,mainclass
                     :projectName ,projectname
                     :classPaths ,classpath
                     :console "internalConsole")))
    (call-interactively #'dape)))


(provide 'init-debugger)

;;; init-debugger.el ends here

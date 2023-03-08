;; init-lang-clojure.el --- Clojure Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Clojure Lang Config
;;

;;; Code:

;; Install clj: https://clojure.org/guides/getting_started#_installation_on_linux
;; Install cider-nrepl: https://docs.cider.mx/cider-nrepl/usage.html#_via_leiningen
;; Install shadow-cljs: https://shadow-cljs.org/
;; Install clojure-lsp: https://github.com/clojure-lsp/clojure-lsp/releases
;; clojurescript: https://clojurescript.org/
;; shadow-cljs repl connect js runtime: https://shadow-cljs.github.io/docs/UsersGuide.html#repl-troubleshooting
(use-package clojure-mode
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . +clojure/setup)
  :config
  (setq clojure-toplevel-inside-comment-form t)

  (defun +clojure/setup ()
    (when (and buffer-file-name
               (member (file-name-extension buffer-file-name) '("clj" "cljs" "cljc")))
      (eglot-ensure)))

  (dolist (mode '(clojure-mode clojurescript-mode))
    (+eglot/set-leader-keys (intern (format "%s-map" mode))))

  (with-eval-after-load 'eglot
    (cl-defmethod eglot-initialization-options (eglot-lsp-server &context (major-mode clojure-mode))
      ;; NOTE: :zip scheme no response from the server?
      '(:dependency-scheme "jar"))

    ;; https://github.com/emacs-lsp/lsp-mode/blob/d3bc47bde5ffc1bace40122a6ec0c6d8b9e84500/clients/lsp-clojure.el#L272
    ;; https://github.com/clojure-lsp/clojure-lsp/blob/master/lib/test/clojure_lsp/shared_test.clj
    (cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode clojure-mode))
      "Support Clojure-lsp `zifile://', `jar:file://' uri scheme."
      (when-let* ((clj-scheme-p (or (string-prefix-p "jar:file://" uri)
                                    (string-prefix-p "zipfile://" uri)))
                  (_ (string-match "^\\(jar:file\\|zipfile\\)://.+\\(!/\\|::\\)\\(.+\\)" uri))
                  (ns-path (match-string 3 uri))
                  (filename (replace-regexp-in-string "/" "." ns-path))
                  (source-dir (file-name-concat (project-root (eglot--current-project)) ".eglot"))
                  (source-file (expand-file-name (file-name-concat source-dir filename))))
        (unless (file-directory-p source-dir)
          (make-directory source-dir t))
        (unless (file-readable-p source-file)
          (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                          :clojure/dependencyContents
                                          (list :uri uri))))
            (with-temp-file source-file (insert content))))
        (puthash source-file uri eglot-path-uri-hashtable)
        source-file))))

;; https://cider.mx/
;; `cider-insert-last-sexp-in-repl'
(use-package cider
  :defer t)

(use-package sayid
  :after cider
  :config
  ;; FIXME: temporary fixed clojure-cli dependencies
  (add-to-list 'cider-jack-in-dependencies `("com.billpiel/sayid" ,sayid-injected-plugin-version)))

;; TODO: neil, A CLI to add common aliases and features to deps.edn-based projects.
;; https://github.com/babashka/neil


(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here

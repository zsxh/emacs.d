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

  (unless (featurep 'eglot)
    (require 'eglot))

  (dolist (mode '(clojure-mode clojurescript-mode))
    (+eglot/set-leader-keys (intern (format "%s-map" mode))))

  (cl-defmethod eglot-initialization-options ((server eglot-lsp-server) &context (major-mode clojure-mode))
    ;; NOTE: :zip scheme no response from the server?
    '(:dependency-scheme "jar"))

  (defun +clojure/setup ()
    (when (and buffer-file-name
               (member (file-name-extension buffer-file-name) '("clj" "cljs" "cljc")))
      (eglot-ensure))))

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

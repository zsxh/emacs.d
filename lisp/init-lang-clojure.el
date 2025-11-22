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
    (+eglot/set-leader-keys (intern (format "%s-map" mode)))))

;; https://cider.mx/
;; `cider-insert-last-sexp-in-repl'
;; (use-package cider
;;   :defer t)

;; Open project dependencies inside jar archives, pairs well with eglot
;; https://git.sr.ht/~dannyfreeman/jarchive
(use-package jarchive
  :defer t
  :init
  (with-eval-after-load 'clojure-mode (jarchive-mode))
  (with-eval-after-load 'clojure-ts-mode (jarchive-mode)))


(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here

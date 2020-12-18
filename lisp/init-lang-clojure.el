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
;; Install clojure lsp server:
;; 1) `lsp-install-server' lsp-mode auto install server
;; 2) https://github.com/snoe/clojure-lsp#installation and set `lsp-clojure-server-store-path'
;;
;; clojurescript: https://clojurescript.org/
;; shadow-cljs repl connect js runtime: https://shadow-cljs.github.io/docs/UsersGuide.html#repl-troubleshooting

(use-package clojure-mode
    :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
           ("\\.cljc\\'" . clojurec-mode)
           ("\\.cljs\\'" . clojurescript-mode))
    :hook ((clojure-mode clojurec-mode clojurescript-mode) . +clojure/lsp)
    :config
    (setq clojure-toplevel-inside-comment-form t)
    (require 'lsp-clojure)
    (dolist (mode-map '(clojure-mode-map clojurec-mode-map clojurescript-mode-map))
      (+language-server/set-common-leader-keys (symbol-value mode-map)))
    (defun +clojure/lsp ()
      (if (and buffer-file-name
               (member (file-name-extension buffer-file-name) '("clj" "cljs" "cljc")))
          (lsp-deferred))))

;; https://cider.mx/
(use-package cider
  :defer t)

;; TODO: clojure debugger
;; https://github.com/clojure-emacs/sayid
(use-package sayid
  :after clojure-mode
  :preface
  ;; FIXME: sayid-version gccemacs `load-true-file-name'
  (advice-add 'sayid--inject-jack-in-dependencies
              :before
              (lambda ()
                (setq sayid-version
                      (let* ((dir (cl-find-if
                                   (lambda (dirname) (string-prefix-p "sayid" dirname))
                                   (directory-files (expand-file-name "elpa" user-emacs-directory))))
                             (file (concat user-emacs-directory "elpa/" dir "/sayid.el")))
                        (lm-version file)))))
  :config
  (sayid-setup-package))


(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here

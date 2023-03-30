;; init-lang-sql.el --- SQL Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  SQL Configurations
;;

;;; Code:

;; use the SQL indent support features of sql-indent.
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; emacsql use emacs built-in sqlite
(use-package emacsql-sqlite-builtin
  :if (>= emacs-major-version 29)
  :defer t)

;; `sqlite-mode-open-file'
;; (use-package sqlite-mode
;;   :ensure nil
;;   :defer t)


(provide 'init-lang-sql)

;;; init-lang-sql.el ends here

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


(provide 'init-lang-sql)

;;; init-lang-sql.el ends here

;; init-c.el --- C/C++ Comfigurations	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  C/C++ Configurations
;;

;;; Code:

;; Fixme: Recompile ccls, current ccls cant not found <iostream>
;; (use-package ccls
;;   :ensure t
;;   :commands lsp-ccls-enable
;;   :init
;;   (defun ccls//enable ()
;;     (condition-case nil
;;         (lsp-ccls-enable)
;;       (user-error nil))
;;     )
;;   :hook ((c-mode . (lambda ()
;;                      (require 'init-lsp)
;;                      (ccls//enable)))
;;          (c++-mode . (lambda ()
;;                        (require 'init-lsp)
;;                        (ccls//enable))))
;;   :config
;;   (setq ccls-executable "~/Code/ccls/release/ccls")
;;   ;; Log file
;;   ;; (setq ccls-extra-args '("--log-file=/tmp/cq.log"))
;;   ;; Cache directory, both relative and absolute paths are supported
;;   ;; (setq ccls-cache-dir ".cquery_cached_index")
;;   ;; Initialization options
;;   (setq ccls-extra-init-params
;;         '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))

(use-package cquery
  :ensure t
  :commands lsp-cquery-enable
  :init
  (defun cquery//enable ()
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil)))
  ;; By default files ending in .h are treated as c files rather than c++ files.
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :hook ((c-mode . (lambda ()
                     (require 'init-lsp)
                     (cquery//enable)))
         (c++-mode . (lambda ()
                       (require 'init-lsp)
                       (cquery//enable))))
  :config
  (setq ccls-executable "/usr/bin/cquery")
  (setq cquery-extra-init-params
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))

(provide 'init-c)

;;; init-c.el ends here

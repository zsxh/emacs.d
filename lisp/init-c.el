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

(use-package cquery
  :ensure t
  :commands lsp-cquery-enable
  :init
  (defun cquery//enable ()
    (condition-case nil
        (lsp-cquery-enable)
        (user-error nil)))
  :hook (c-mode-common . (lambda ()
                           (require 'init-lsp)
                           (cquery//enable)))
  :config
  (setq cquery-executable "/usr/bin/cquery")

  ;; Log file
  ;; (setq cquery-extra-args '("--log-file=/tmp/cq.log"))

  ;; Cache directory, both relative and absolute paths are supported
  ;; (setq cquery-cache-dir ".cquery_cached_index")

  ;; Initialization options
  (setq cquery-extra-init-params
        '(:index (:comment 2) :cacheFormat "msgpack" :completion (:detailedLabel t))))


(provide 'init-c)

;;; init-c.el ends here

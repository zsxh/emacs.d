;; init-pyim.el --- Chinese Pingyin Input Method Configuration	-*- lexical-binding: t -*-

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
;;  Pingyin Configuration
;;

;;; Code:

(defun +pyim/toggle ()
  "Toggle pyim."
  (interactive)
  (if (featurep 'pyim)
      (toggle-input-method)
    (+pyin/start)
    (toggle-input-method)))

(global-set-key "\C-\\" '+pyim/toggle)
(global-set-key (kbd "C-M-\\") 'pyim-convert-code-at-point)

;;;###autoload
(defun +pyin/start ()
  "Start pyim."
  (interactive)

  (use-package pyim
    :ensure t
    :demand t
    ;; :bind (("C-\\" . toggle-input-method))
    :config
    ;; 激活 basedict 拼音词库
    (use-package pyim-basedict
      :ensure t
      :config (pyim-basedict-enable))

    ;; 五笔用户使用 wbdict 词库
    ;; (use-package pyim-wbdict
    ;;   :ensure nil
    ;;   :config (pyim-wbdict-gbk-enable))

    (setq default-input-method "pyim")

    ;; 我使用全拼
    (setq pyim-default-scheme 'quanpin)

    ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
    ;; 我自己使用的中英文动态切换规则是：
    ;; 1. 光标只有在注释里面时，才可以输入中文。
    ;; 2. 光标前是汉字字符时，才能输入中文。
    ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-dynamic-english
                    pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    ;; 开启拼音搜索功能
    (pyim-isearch-mode 1)

    ;; 使用 popup.el 来绘制选词框, 如果用 emacs26, 建议设置
    ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
    ;; 手动安装 posframe 包。
    (if (display-graphic-p)
        (setq pyim-page-tooltip 'posframe)
      (setq pyim-page-tooltip 'poppup))

    ;; 选词框显示5个候选词
    (setq pyim-page-length 7)

    ;; 让 Emacs 启动时自动加载 pyim 词库
    ;; (add-hook 'emacs-startup-hook
    ;;           #'(lambda () (pyim-restart-1 t)))
    (lambda () (pyim-restart-1 t))))


(provide 'init-pyim)

;;; init-pyim.el ends here

;; init-window.el --- Window Setup	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Window Setup
;;

;;; Code:

;; TODO: `display-buffer-alist' https://youtu.be/E-xUNlZi3rI?t=948
;; TODO: Make an Emacs Buffer Open the Way You Want, https://lambdaland.org/posts/2022-12-27_repl_buffer_on_the_right/

;;;;;;;;;;;;;; Layout ;;;;;;;;;;;;;;

(use-package persp-mode
  :commands (persp-switch
             persp-add-new
             persp-switch-to-buffer
             get-current-persp
             persp-contain-buffer-p
             +persp/add-new-with-visible-buffers)
  :custom
  (persp-auto-save-persps-to-their-file nil)
  (persp-auto-save-opt 0)
  (persp-auto-resume-time 0)
  (persp-init-frame-behaviour t)
  :config
  (persp-mode t)
  ;; [fix] let*: Symbol’s function definition is void: \(setf\ persp-window-conf\)
  ;; https://emacs-china.org/t/topic/6416
  (eval '(byte-compile
          (defun +persp/add-new-with-visible-buffers (name)
            "Create new perspective and add all visible buffers into it."
            (interactive "sA name for the new perspective: ")
            (let ((p (persp-add-new name)))
              (dolist (win (window-list))
                (persp-add-buffer (window-buffer win) p))
              (setf (persp-window-conf p)
                    (funcall persp-window-state-get-function (selected-frame)))
              (persp-switch name))))))


(provide 'init-window)

;;; init-window.el ends here

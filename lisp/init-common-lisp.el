;; init-common-lisp.el --- Common Lisp Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Common Lisp Configurations
;;

;;; Code:

(use-package sly
  :ensure t
  :hook ((lisp-mode . sly-mode)
         (sly-mode . +common-lisp/init-sly)
         (sly-mode . +common-lisp/sly-config))
  :config
  (setq inferior-lisp-program "sbcl")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'sly-db-mode 'emacs)))

(defun +common-lisp/sly-config ()
  (+funcs/major-mode-leader-keys
   lisp-mode-map
   "'" '(sly-mrepl :which-key "repl")
   "g" '(nil :which-key "goto")
   "gd" '(sly-edit-definition "" :which-key "goto-definition")
   "m" '(nil :which-key "macro")
   "ms" '(macrostep-expand :which-key "macrostep-expand"))

  (with-eval-after-load 'evil
    (evil-define-key 'normal sly-xref-mode-map
      (kbd "RET") 'sly-goto-xref)
    (evil-define-key 'normal sly-mrepl-mode-map
      "q" 'quit-window)))

(use-package sly-macrostep
  :ensure t
  :commands macrostep-expand
  :bind ((:map lisp-mode-map
               ("C-c e" . macrostep-expand))))

(defun +common-lisp/init-sly ()
  "Attempt to auto-start sly when opening a Lisp buffer."
  (cond ((sly-connected-p))
        ((executable-find inferior-lisp-program)
         (let ((sly-auto-start 'always))
           (sly-auto-start)
           (add-hook 'kill-buffer-hook #'+common-lisp/cleanup-sly-maybe nil t)))
        ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
                  inferior-lisp-program))))

(defun +common-lisp/cleanup-sly-maybe ()
  "Kill processes and leftover buffers when killing the last sly buffer."
  (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                   if (and (buffer-local-value 'sly-mode buf)
                           (get-buffer-window buf))
                   return t)
    (dolist (conn (sly--purge-connections))
      (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
    (let (kill-buffer-hook kill-buffer-query-functions)
      (mapc #'kill-buffer
            (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (buffer-local-value 'sly-mode buf)
                     collect buf)))))


(provide 'init-common-lisp)

;;; init-common-lisp.el ends here

;; init-lang-moonbit.el --- MoonBit Configuration 	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  MoonBit Configuration
;;

;;; Code:

(use-package moonbit-mode
  :vc (:url "https://github.com/zsxh/moonbit-mode.git" :branch "feat/emacs-30")
  ;; :vc (:url "https://github.com/cxa/moonbit-mode.git")
  ;; :load-path "~/workspace/emacs/moonbit-mode"
  ;; :mode ("\\.mbt\\'" . moonbit-mode)
  :init
  (defun moonbit-setup ()
    (setq-local tab-width 2))
  :hook ((moonbit-mode . eglot-ensure)
         (moonbit-mode . moonbit-setup))
  :config
  (add-to-list
   'treesit-language-source-alist
   '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit.git"))
  (unless (treesit-language-available-p 'moonbit)
    (treesit-install-language-grammar 'moonbit))

  (require 'eglot)
  (push `(moonbit-mode . ("moonbit-lsp" "--stdio")) eglot-server-programs)
  (+eglot/set-leader-keys moonbit-mode-map))

(with-eval-after-load 'moonbit-mode
  (defun +moonbit/execute-command (server command arguments)
    (pcase command
      ("moonbit-lsp/test" (moonbit--lsp/test server arguments))
      ("moonbit-lsp/trace-test" (message "Unhandled method %s" command))
      ("moonbit-lsp/format-toplevel" (moonbit--lsp/format server arguments))
      ("moonbit-ai/generate" (message "Unhandled method %s" command))
      ("moonbit-ai/generate-batched" (message "Unhandled method %s" command))))

  (defun moonbit--lsp/format (server arguments)
    (let* ((arg (aref arguments 0))
           (region (eglot-range-region (plist-get arg :range)))
           (beg (car region))
           (end (cdr region))
           (result (jsonrpc-request server
                                    :moonbit-lsp/format
                                    (list :content (buffer-substring-no-properties beg end)
                                          :blockStyle :json-false))))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        ;; NOTE: Why does `moonbit-lsp/format` keep adding a newline to the end?
        (insert (substring result 0 (1- (length result))))
        (if eglot-codelens-mode (call-interactively 'eglot-codelens-force-refresh-lens)))))

  (defun moonbit--lsp/test (server arguments)
    (let ((default-directory (+project/root))
          (cmd))
      (cl-destructuring-bind
          (&key backend pkgPath fileName index update debug &allow-other-keys)
          (aref arguments 0)
        (if (eq debug t)
            (message "Debug not supported")
          (compile
           (concat "moon test"
                   (when pkgPath (format " -p %s" pkgPath))
                   (when fileName (format " -f %s" fileName))
                   (when index (format " -i %d" index))
                   (when (eq update t) " -u")
                   (when backend (format " --target %s" backend)))))))))


(provide 'init-lang-moonbit)

;;; init-lang-moonbit.el ends here

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
    (setq-local tab-width 2)
    (setq-local face-remapping-alist
                '((eglot-semantic-async
                   :weight normal
                   :slant italic
                   :underline t))))
  :hook ((moonbit-mode . eglot-ensure)
         (moonbit-mode . moonbit-setup))
  :config
  ;; (add-to-list
  ;;  'treesit-language-source-alist
  ;;  '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit.git"))
  (unless (treesit-language-available-p 'moonbit)
    (treesit-install-language-grammar 'moonbit))

  (require 'eglot)
  (push `(moonbit-mode . ("moonbit-lsp" "--stdio")) eglot-server-programs)
  (+eglot/set-leader-keys moonbit-mode-map))

(with-eval-after-load 'moonbit-mode
  (cl-defmethod eglot-execute :around
    (server action &context (major-mode moonbit-mode))
    "Custom handler for performing client commands."
    (let ((command (plist-get action :command))
          (arguments (plist-get action :arguments)))
      (pcase command
        ("moonbit-lsp/format-nth-toplevel" (moonbit--lsp/format server arguments))
        ("moonbit-lsp/run-test" (moonbit--lsp/test arguments))
        ("moonbit-lsp/debug-test" (message "Unhandled method %s" command))
        ("moonbit-lsp/update-test" (moonbit--lsp/test arguments 'update))
        ("moonbit-lsp/trace-test" (message "Unhandled method %s" command))
        ("moonbit-lsp/run-all-tests" (moonbit--lsp/test arguments))
        ("moonbit-lsp/update-all-tests" (moonbit--lsp/test arguments 'update))
        ("moonbit-lsp/run-main" (moonbit--lsp/main arguments))
        ("moonbit-lsp/debug-main" (message "Unhandled method %s" command)) ;; moon build --debug --target js
        ("moonbit-lsp/trace-main" (message "Unhandled method %s" command))
        ("moonbit-ai/generate" (message "Unhandled method %s" command))
        ("moonbit-ai/generate-batched" (message "Unhandled method %s" command))
        (_ (cl-call-next-method)))))

  (defun moonbit--lsp/format (server arguments)
    (let* ((arg (aref arguments 0))
           (n (plist-get arg :n))
           (result (eglot--request
                    server
                    :moonbit-lsp/format-nth
                    (list :content (buffer-substring-no-properties (point-min) (point-max))
                          :n n)))
           (region (eglot-range-region (plist-get result :range)))
           (beg (car region))
           (end (cdr region))
           (newtext (plist-get result :newText)))
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert (substring newtext 0 (length newtext))))))

  (defun moonbit--lsp/test (arguments &optional action)
    (let ((default-directory (+project/root)))
      (cl-destructuring-bind
          (&key cwdUri pkgPath fileUri fileName backend toplevelIndex index testName &allow-other-keys)
          (aref arguments 0)
        (compile
         (concat "moon test"
                 (when pkgPath (format " -p %s" pkgPath))
                 (when fileName (format " -f %s" fileName))
                 (when index (format " -i %d" index))
                 (cond
                  ((eq action 'update) " -u")
                  ((eq action 'debug) " -g")
                  (t nil))
                 (when backend (format " --target %s" backend)))))))

  ;; TODO: get target backend and pkg name
  (defun moonbit--lsp/main (arguments)
    (let ((default-directory (+project/root)))
      (cl-destructuring-bind
          (&key modUri pkgUri pkgPath fileUri &allow-other-keys)
          (aref arguments 0)
        (compile
         (concat "moon run "
                 (substring pkgUri (1+ (length modUri))))))))

  (with-eval-after-load 'compile
    ;; MoonBit 方框诊断
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(moonbit-box
                   "^\\s-*╭─\\[\\s-*\\([^]\n]+?\\):\\([0-9]+\\):\\([0-9]+\\)\\s-*\\]"
                   1 2 3))
    ;; MoonBit test 失败
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(moonbit-test
                   "failed:\\s-+\\([^\n]+?\\):\\([0-9]+\\):\\([0-9]+\\)"
                   1 2 3))
    (dolist (r '(moonbit-box moonbit-test))
      (add-to-list 'compilation-error-regexp-alist r))))

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist
               '(moonbit-mode nerd-icons-mdicon "nf-md-rabbit_variant" :face nerd-icons-maroon))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("mbt" nerd-icons-mdicon "nf-md-rabbit_variant" :face nerd-icons-maroon)))

(provide 'init-lang-moonbit)

;;; init-lang-moonbit.el ends here

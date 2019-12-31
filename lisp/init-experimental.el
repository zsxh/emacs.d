;; init-experimental.el --- Experimental Feature	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Experimental Feature
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Use system-wide command instead now
;; Simplified and community-driven man pages
;; (use-package tldr
;;   :ensure t
;;   :commands tldr)


;; FIXME: wait until emacs xwdigets work well ...
(use-package webkit-katex-render
  :if (featurep 'xwidget-internal)
  :quelpa ((webkit-katex-render
            :fetcher github
            :repo "fuxialexander/emacs-webkit-katex-render"
            :files (:defaults "katex.html")))
  :commands webkit-katex-render-mode
  :config
  (with-eval-after-load 'doom-themes
    (setq webkit-katex-render--background-color (doom-color 'bg)))

  (defun webkit-katex-render--org-math-at-point ()
    (if (org-inside-LaTeX-fragment-p)
        (let (beg end)
          (let ((datum (org-element-context)))
            (when (memq (org-element-type datum)
                        '(latex-environment latex-fragment))
              (setq beg (org-element-property :begin datum))
              (setq end (org-element-property :end datum))
              (save-excursion
                (goto-char beg)
                (let* ((context (org-element-context))
                       (type (org-element-type context)))
                  (when (memq type '(latex-environment latex-fragment))
                    (let ((value (org-element-property :value context))
                          (beg (org-element-property :begin context))
                          (end (save-excursion
                                 (goto-char (org-element-property :end context))
                                 (skip-chars-backward " \r\t\n")
                                 (point))))
                      (goto-char end)
                      (list end (funcall webkit-katex-render--org-math-preprocess-function
                                         value type)))))))))
      nil))

  (defun webkit-katex-render--math-at-point ()
    "Return recognized math at point."
    (condition-case err
        (or (and (equal major-mode 'latex-mode)
                 (webkit-katex-render--tex-math-at-point))
            (and (equal major-mode 'org-mode)
                 (webkit-katex-render--org-math-at-point))
            (when webkit-katex-render--math-at-point-function
              (funcall webkit-katex-render--math-at-point-function)))
      (error
       (message "[Error] webkit-kate-render--math-at-point, %s" (error-message-string err))
       nil)))

  (defun webkit-katex-render-cleanup ()
    "Destroy color picker buffer and frame."
    (dolist (xwidget-view xwidget-view-list)
      (delete-xwidget-view xwidget-view))
    (posframe-delete-all)
    (when (buffer-live-p webkit-katex-render--buffer-name)
      (kill-buffer webkit-katex-render--buffer-name))))


(provide 'init-experimental)

;;; init-experimental.el ends here

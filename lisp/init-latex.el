;; init-latex.el --- Latex Configuration	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Latex Configuration
;;

;;; Code:

;; require TeX Live installation
;; pacman -S texlive-bin texlive-core texlive-latexextra texlive-langchinese
(use-package tex
  :ensure auctex
  :defer t
  :hook (LaTeX-mode . +latex/config)
  :config
  (defun +latex/config ()
    (turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    (reftex-isearch-minor-mode)
    (setq TeX-PDF-mode t)
    (setq TeX-source-correlate-method 'synctex)
    (setq TeX-source-correlate-start-server t))

  ;; to use pdfview with auctex
  ;; (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
  ;;                             TeX-source-correlate-start-server t)
  ;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package company-auctex
  :ensure t
  :after tex
  :hook (LaTeX-mode . (lambda ()
                        (make-local-variable 'company-backends)
                        (company-auctex-init))))

;; org-latex edit, preview, export ...

(use-package org-edit-latex
  :defer t
  :after org)

(use-package org2ctex
  :ensure t
  :after org
  :config
  (org2ctex-mode))

;; TODO: org latex fragment preview
(with-eval-after-load 'org
  (defvar org-latex-fragment-last nil
    "Holds last fragment/environment you were on.")

  (defun org-latex-fragment-toggle ()
    "Toggle a latex fragment image "
    (interactive)
    (and (eq 'org-mode major-mode)
         (let* ((el (org-element-context))
                (el-type (car el)))
           (cond
            ;; were on a fragment and now on a new fragment
            ((and
              ;; fragment we were on
              org-latex-fragment-last
              ;; and are on a fragment now
              (or
               (eq 'latex-fragment el-type)
               (eq 'latex-environment el-type))
              ;; but not on the last one this is a little tricky. as you edit the
              ;; fragment, it is not equal to the last one. We use the begin
              ;; property which is less likely to change for the comparison.
              (not (= (org-element-property :begin el)
                      (org-element-property :begin org-latex-fragment-last))))
             ;; go back to last one and put image back
             (save-excursion
               (goto-char (org-element-property :begin org-latex-fragment-last))
               (org-preview-latex-fragment))
             ;; now remove current image
             (goto-char (org-element-property :begin el))
             (let ((ov (cl-loop for ov in (org--list-latex-overlays)
                                if
                                (and
                                 (<= (overlay-start ov) (point))
                                 (>= (overlay-end ov) (point)))
                                return ov)))
               (when ov
                 (delete-overlay ov)))
             ;; and save new fragment
             (setq org-latex-fragment-last el))

            ;; were on a fragment and now are not on a fragment
            ((and
              ;; not on a fragment now
              (not (or
                    (eq 'latex-fragment el-type)
                    (eq 'latex-environment el-type)))
              ;; but we were on one
              org-latex-fragment-last)
             ;; put image back on
             (save-excursion
               (goto-char (org-element-property :begin org-latex-fragment-last))
               (org-preview-latex-fragment))
             ;; unset last fragment
             (setq org-latex-fragment-last nil))

            ;; were not on a fragment, and now are
            ((and
              ;; we were not one one
              (not org-latex-fragment-last)
              ;; but now we are
              (or
               (eq 'latex-fragment el-type)
               (eq 'latex-environment el-type)))
             (goto-char (org-element-property :begin el))
             ;; remove image
             (let ((ov (cl-loop for ov in (org--list-latex-overlays)
                                if
                                (and
                                 (<= (overlay-start ov) (point))
                                 (>= (overlay-end ov) (point)))
                                return ov)))
               (when ov
                 (delete-overlay ov)))
             (setq org-latex-fragment-last el))))))

  ;; (add-hook 'post-command-hook 'org-latex-fragment-toggle)

  (defun update-org-latex-fragments ()
    (org-latex-preview)
    (plist-put org-format-latex-options :scale (+ text-scale-mode-amount 2))
    (org-latex-preview))

  ;; (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)
  )


(provide 'init-latex)

;;; init-latex.el ends here

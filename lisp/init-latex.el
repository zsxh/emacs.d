;; init-latex.el --- Latex Configuration	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Latex Configuration
;;

;;; Code:

(use-package tex
  :ensure auctex
  :hook (tex-mode . +latex/config)
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
  :after tex
  :hook (LaTeX-mode . (lambda ()
                        (make-local-variable 'company-backends)
                        (company-auctex-init))))

;; TODO: org-latex-impatient
;; https://github.com/yangsheng6810/org-latex-impatient

;; org-latex edit, preview, export ...

;; require TeX Live installation
;; $sudo pacman -S texlive-bin texlive-core texlive-latexextra texlive-langchinese
;; org svg export to pdf also require 'inkscape' and '#+latex_header_extra: \usepackage{svg}'
;; $sudo pacman -S inkscape
(use-package org2ctex
  :after org
  :commands (org2ctex-mode)
  :init
  (add-hook 'org-load-hook (lambda () (org2ctex-mode)))
  :config
  (setq org2ctex-latex-commands
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package company-math
  :after org
  :hook (org-mode . +company-math/setup)
  :config
  (setq company-math-allow-latex-symbols-in-faces t)
  ;; local configuration for TeX modes
  (defun +company-math/setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands))
                        company-backends))))

(defun +latex/org-setup ()
  ;; https://ivanaf.com/automatic_latex_fragment_toggling_in_org-mode.html
  (defvar-local org-latex-fragment-last nil
    "Holds last fragment/environment you were on.")

  (defun my/org-latex-fragment--get-current-latex-fragment ()
    "Return the overlay associated with the image under point."
    (car (--select (eq (overlay-get it 'org-overlay-type) 'org-latex-overlay) (overlays-at (point)))))

  (defun my/org-in-latex-fragment-p ()
    "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
    (let* ((el (org-element-context))
           (el-type (car el)))
      (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
           (org-element-property :begin el))))

  (defun org-latex-fragment-toggle-auto ()
    ;; Wait for the s
    (interactive)
    (while-no-input
      (run-with-idle-timer 0.05 nil 'org-latex-fragment-toggle-helper)))

  (defun org-latex-fragment-toggle-helper ()
    "Toggle a latex fragment image "
    (condition-case nil
        (and (eq 'org-mode major-mode)
             (let* ((begin (my/org-in-latex-fragment-p)))
               (cond
                ;; were on a fragment and now on a new fragment
                ((and
                  ;; fragment we were on
                  org-latex-fragment-last
                  ;; and are on a fragment now
                  begin
                  ;; but not on the last one this is a little tricky. as you edit the
                  ;; fragment, it is not equal to the last one. We use the begin
                  ;; property which is less likely to change for the comparison.
                  (not (= begin
                          org-latex-fragment-last)))
                 ;; go back to last one and put image back
                 (save-excursion
                   (goto-char org-latex-fragment-last)
                   (when (my/org-in-latex-fragment-p) (org-latex-preview))
                   ;; now remove current imagea
                   (goto-char begin)
                   (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                     (when ov
                       (delete-overlay ov)))
                   ;; and save new fragment
                   (setq org-latex-fragment-last begin)))

                ;; were on a fragment and now are not on a fragment
                ((and
                  ;; not on a fragment now
                  (not begin)
                  ;; but we were on one
                  org-latex-fragment-last)
                 ;; put image back on
                 (save-excursion
                   (goto-char org-latex-fragment-last)
                   (when (my/org-in-latex-fragment-p) (org-latex-preview)))

                 ;; unset last fragment
                 (setq org-latex-fragment-last nil))

                ;; were not on a fragment, and now are
                ((and
                  ;; we were not one one
                  (not org-latex-fragment-last)
                  ;; but now we are
                  begin)
                 (save-excursion
                   (goto-char begin)
                   ;; remove image
                   (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                     (when ov
                       (delete-overlay ov)))
                   (setq org-latex-fragment-last begin)))
                ;; else not on a fragment
                ((not begin)
                 (setq org-latex-fragment-last nil)))))
      (error nil)))

  (setq org-latex-fragment-toggle-helper (byte-compile 'org-latex-fragment-toggle-helper))
  (setq org-latex-fragment-toggle-auto (byte-compile 'org-latex-fragment-toggle-auto))

  (require 'face-remap)

  (defun update-org-latex-fragments ()
    (org-clear-latex-preview)
    (plist-put org-format-latex-options :scale (+ text-scale-mode-amount 1.5))
    (org-latex-preview '(16)))

  (with-eval-after-load 'face-remap
    (plist-put org-format-latex-options :scale (+ text-scale-mode-amount 1.5)))

  (defun activate-org-latex-preview ()
    (interactive)
    (add-hook 'post-command-hook 'org-latex-fragment-toggle-auto nil t)
    (add-hook 'text-scale-mode-hook 'update-org-latex-fragments nil t)
    (plist-put org-format-latex-options :scale (+ text-scale-mode-amount 2))
    (org-latex-preview '(16)))

  (defun deactivate-org-latex-preview ()
    (interactive)
    (remove-hook 'post-command-hook 'org-latex-fragment-toggle-auto t)
    (remove-hook 'text-scale-mode-hook 'update-org-latex-fragments t)
    (org-clear-latex-preview))

  ;; (add-hook 'org-mode-hook 'activate-org-latex-preview)

  (defvar-local +latex/org-latex-preview-p nil)

  (defun +latex/toggle-latex-preview ()
    (interactive)
    (if +latex/org-latex-preview-p
        (deactivate-org-latex-preview)
      (activate-org-latex-preview))
    (setq +latex/org-latex-preview-p (not +latex/org-latex-preview-p))))

(add-hook 'org-load-hook '+latex/org-setup)

;; TODO: mathpix.el use Mathpix's API to convert clips into latex equations
;; https://github.com/jethrokuan/mathpix.el
(use-package mathpix.el
  :quelpa (mathpix :fetcher github :repo "jethrokuan/mathpix.el")
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "deepin-screen-recorder -s %s"))


(provide 'init-latex)

;;; init-latex.el ends here

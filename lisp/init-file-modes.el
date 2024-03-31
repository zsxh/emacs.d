;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  File Modes
;;

;;; Code:

;; Markdowm
(use-package markdown-mode
  :defer t
  :config
  (+funcs/major-mode-leader-keys
   markdown-mode-map
   "'" '(markdown-edit-code-block :which-key "edit-code-block")
   "T" '(nil :which-key "toggle")
   "Ti" '(markdown-toggle-inline-images :which-key "inline-images")))

;; CSV
(use-package csv-mode :defer t)
(use-package rainbow-csv-mode
  :vc (:url "https://github.com/emacs-vs/rainbow-csv")
  :hook ((csv-mode tsv-mode) . rainbow-csv-mode))

;; Yaml
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . (lambda ()
                       (setq-local completion-at-point-functions
                                   '(cape-dabbrev cape-file cape-keyword cape-abbrev)))))

(with-eval-after-load 'yaml-ts-mode
  (add-hook 'yaml-ts-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          '(cape-dabbrev cape-file cape-keyword cape-abbrev)))))

;; PDF
;;
;; https://github.com/vedang/pdf-tools
;; install poppler
;; - macos: brew install poppler, set PKG_CONFIG_PATH
;; - arch: pacman -S poppler
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (with-eval-after-load 'evil-collection
    (evil-collection-pdf-setup)
    (evil-define-key 'normal pdf-view-mode-map
      (kbd "C-s") 'isearch-forward
      (kbd "C-r") 'isearch-backward
      "d" 'pdf-view-scroll-up-or-next-page
      "u" 'pdf-view-scroll-down-or-previous-page)))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename (locate-user-emacs-file "cache/.pdf-view-restore")))

;; LaTeX
;; TODO: https://github.com/dandavison/xenops
;; NOTE: https://karthinks.com/software/latex-input-for-impatient-scholars/
;; NOTE: karthinks Fast, Async LaTeX Previews https://www.youtube.com/watch?v=n-AfvuV-bYo


(provide 'init-file-modes)

;;; init-file-modes.el ends here

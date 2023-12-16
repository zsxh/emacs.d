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
   "T" '(nil :which-key "toggle")
   "Ti" '(markdown-toggle-inline-images :which-key "inline-images")))

;; CSV
(use-package csv-mode :defer t)
(use-package rainbow-csv-mode
  :vc (:url "https://github.com/emacs-vs/rainbow-csv" :rev :newest)
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

;; LaTeX
;; TODO: https://github.com/dandavison/xenops
;; NOTE: https://karthinks.com/software/latex-input-for-impatient-scholars/
;; NOTE: karthinks Fast, Async LaTeX Previews https://www.youtube.com/watch?v=n-AfvuV-bYo




(provide 'init-file-modes)

;;; init-file-modes.el ends here

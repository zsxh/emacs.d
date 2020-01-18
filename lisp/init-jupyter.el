;; init-jupyter.el --- Emacs Jupyter Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;; Emacs Jupyter Configurations
;;

;;; Code:

;; An interface ot communicate with Jupyter kernels in Emacs
(use-package jupyter
  :commands (jupyter-run-repl jupyter-connect-repl))

(use-package ein
  :commands (ein:notebooklist-open ;; run jupter notebook first
             ein:notebooklist-login
             ein:jupyter-server-start)
  :config
  (use-package poly-ein
    :defer t
    :config
    (setq ein:polymode nil))

  (use-package ein-subpackages
    :defer t
    :config
    (setq ein:completion-backend 'ein:use-company-backend)
    (defun +ein/config ()
      (when (bound-and-true-p rainbow-delimiters-mode)
        (rainbow-delimiters-mode -1))

      (setq-local company-backends '(ein:company-backend company-files company-dabbrev))
      ;; disable company box to improve performance
      (when (bound-and-true-p company-box-mode)
        (company-box-mode -1)))

    (add-hook 'ein:notebook-mode-hook #'+ein/config))

  (use-package ein-notebook
    :defer t
    :bind (("C-<return>" . ein:worksheet-execute-cell)
           ("S-<return>" . ein:worksheet-execute-cell-and-goto-next)
           ("M-j" . ein:worksheet-move-cell-down)
           ("M-k" . ein:worksheet-move-cell-up))
    :config
    ;; use evil-define-minor-mode-key over evil-define-key
    ;; https://github.com/noctuid/evil-guide#mode-specific-keybindings
    (evil-define-minor-mode-key 'normal 'ein:notebook-mode
      "J" 'ein:worksheet-goto-next-input
      "K" 'ein:worksheet-goto-prev-input
      "," 'ipython-notebook-hydra/body))

  (use-package ein-cell
    :defer t
    :config
    (setq ein:cell-traceback-level nil ;; Show all traceback
          ein:slice-image t)

    ;; FIXME: fixed by https://github.com/millejoh/emacs-ipython-notebook/commit/c71075328cf3554e346632418c6efa05d836c413 ?
    ;; you can use 'ansi-color-filter-apply instead of 'ansi-color-apply to escape ansi code
    ;; (advice-add 'ein:output-area-convert-mime-types :around (lambda (orig-fun &rest args)
    ;;                                                           (let* ((json (apply orig-fun args))
    ;;                                                                  (text (plist-get json :text)))
    ;;                                                             (when (plist-member json :text)
    ;;                                                               (plist-put json :text (ansi-color-apply text)))
    ;;                                                             json)))
    )

  (use-package ein-multilang
    :defer t
    :config
    ;; FIXME: ein company backend freeze emacs when auto-completing after [. / \ < > + - * ^] characters (too many candidates)
    ;; so invoke company-complete-common manually after [. / \ < > + - * ^] characters
    (defun +ein/julia-complete-delay ()
      (if (memq (char-before) '(?\\ ?/ ?. ?< ?> ?+ ?- ?* ?^))
          nil
        0.01))

    (defun +ein/julia-extra-setup ()
      (setq-local
       indent-line-function
       (apply-partially #'ein:ml-indent-line-function
                        (lambda ()
                          (julia-indent-line)
                          (when (and (eq ?. (char-before)) company-mode)
                            (company-complete-common)))))
      (when company-mode
        (setq-local company-minimum-prefix-length 2)
        (setq-local company-idle-delay '+ein/julia-complete-delay)))

    (advice-add 'ein:ml-lang-setup-julia :after '+ein/julia-extra-setup)

    (defun ein:ml-lang-setup-rust ()
      (when (featurep 'rust-mode)
        (setq-local mode-name "EIN[rust]")
        (setq-local comment-start "// ")
        (setq-local indent-line-function
                    (apply-partially #'ein:ml-indent-line-function #'rust-mode-indent-line))
        (when (boundp 'rust-mode-syntax-table)
          (set-syntax-table rust-mode-syntax-table)))))

  (use-package ein-traceback
    :defer t
    :config
    (+funcs/major-mode-leader-keys ein:traceback-mode-map
                                   "RET" 'ein:tb-jump-to-source-at-point-command
                                   "n" 'ein:tb-next-item
                                   "p" 'ein:tb-prev-item
                                   "q" 'bury-buffer))

  (defun +ein/ein:worksheet-merge-cell-next ()
    (interactive)
    (ein:worksheet-merge-cell (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

  (defhydra ipython-notebook-hydra (:hint nil :idle 1)
    "
 Operations on Cells^^^^^^^^               On Worksheets^^^^            Other
 ----------------------------^^^^^^^^      ------------------------^^^^ ----------------------------------^^^^
 [_k_/_j_]^     select prev/next        ^^^[_h_/_l_]  select prev/next  [_t_]^^         toggle output
 [_K_/_J_]^     move up/down            ^^^[_H_/_L_]  move left/right   [_C-l_/_C-S-l_] clear/clear all output
 [_C-k_/_C-j_]^ merge above/below       ^^^[_1_.._9_] open [1st..last]  [_C-o_]^^       open console
 [_O_/_o_]^     insert above/below      ^^^[_+_/_-_]  create/delete     [_C-s_/_C-r_]   save/rename notebook
 [_y_/_p_/_d_/_s_] copy/paste/delete/split [_R_]^^ rename worksheet     [_x_/_C-S-r_]   close/restart notebook
 [_u_]^^^       change type             ^^^^^^^                         [_q_]^^         quit
 [_RET_/_M-RET_/_Z_] execute/exec all/interrupt"
    ("h" ein:notebook-worksheet-open-prev-or-last)
    ("j" ein:worksheet-goto-next-input)
    ("k" ein:worksheet-goto-prev-input)
    ("l" ein:notebook-worksheet-open-next-or-first)
    ("H" ein:notebook-worksheet-move-prev)
    ("J" ein:worksheet-move-cell-down)
    ("K" ein:worksheet-move-cell-up)
    ("L" ein:notebook-worksheet-move-next)
    ("t" ein:worksheet-toggle-output)
    ("d" ein:worksheet-kill-cell :exit t)
    ("R" ein:worksheet-rename-sheet)
    ("y" ein:worksheet-copy-cell)
    ("p" ein:worksheet-yank-cell)
    ("o" ein:worksheet-insert-cell-below :exit t)
    ("O" ein:worksheet-insert-cell-above :exit t)
    ("u" ein:worksheet-change-cell-type :exit t)
    ("RET" ein:worksheet-execute-cell-and-goto-next)
    ("M-RET" ein:worksheet-execute-all-cell :exit t)
    ("Z" ein:notebook-kernel-interrupt-command)
    ;; Output
    ("C-l" ein:worksheet-clear-output)
    ("C-S-l" ein:worksheet-clear-all-output)
    ;;Console
    ("C-o" ein:console-open)
    ;; Merge and split cells
    ("C-k" ein:worksheet-merge-cell)
    ("C-j" +ein/ein:worksheet-merge-cell-next)
    ("s" ein:worksheet-split-cell-at-point)
    ;; Notebook
    ("C-s" ein:notebook-save-notebook-command :exit t)
    ("C-r" ein:notebook-rename-command :exit t)
    ("C-S-r" ein:notebook-restart-session-command :exit t)
    ("1" ein:notebook-worksheet-open-1th)
    ("2" ein:notebook-worksheet-open-2th)
    ("3" ein:notebook-worksheet-open-3th)
    ("4" ein:notebook-worksheet-open-4th)
    ("5" ein:notebook-worksheet-open-5th)
    ("6" ein:notebook-worksheet-open-6th)
    ("7" ein:notebook-worksheet-open-7th)
    ("8" ein:notebook-worksheet-open-8th)
    ("9" ein:notebook-worksheet-open-last)
    ("+" ein:notebook-worksheet-insert-next)
    ("-" ein:notebook-worksheet-delete)
    ("x" ein:notebook-close :exit t)
    ("q" nil :exit t)))

(use-package magic-latex-buffer
  :commands magic-latex-buffer
  :config
  (with-eval-after-load 'ein-multilang
    (+funcs/major-mode-leader-keys
     ein:notebook-multilang-mode-map
     "m" '(magic-latex-buffer :which-key "toggle-latex-preview"))))


(provide 'init-jupyter)

;;; init-jupyter.el ends here

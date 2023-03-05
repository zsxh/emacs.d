;; init-eaf.el --- Emacs Application Framework	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Application Framework
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; cd ~/.emacs.d
;; git submodule add --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/submodules/emacs-application-framework/
;; cd ~/.emacs.d/submodules/emacs-application-framework
;; git clone --depth 1 --single-branch https://github.com/emacs-eaf/eaf-browser.git app/eaf-browser
;; git clone --depth 1 --single-branch https://github.com/emacs-eaf/eaf-pdf-viewer.git app/eaf-pdf-viewer
;; git clone --depth 1 --single-branch https://github.com/emacs-eaf/eaf-image-viewer.git app/eaf-image-viewer
;;
;; python dependencies, check dependencies.json
;; core: pip install --user --upgrade epc sexpdata tld lxml PyQt6 PyQt6-Qt6 PyQt6-sip PyQt6-WebEngine PyQt6-WebEngine-Qt6
;; browser: pip install --user --upgrade pysocks braveblock
;; pdf: pip install --user --upgrade packaging pymupdf
;;
;; js Dependencies, check package.json
(use-package eaf
  :load-path "~/.emacs.d/submodules/emacs-application-framework"
  :commands (eaf-open
             eaf-open-browser
             eaf-open-browser-with-history
             eaf-get-file-name-extension)
  :init
  (defun +eaf/find-file (orig-fn &rest args)
    "Find file advice"
    (let* ((file (car args))
           (file-extension (file-name-extension file))
           (ext (if file-extension (downcase file-extension) nil)))
      (cond
       ((not ext) (apply orig-fn args))
       ((member (eaf-get-file-name-extension file) eaf-office-extension-list)
        (eaf-open-office file))
       ((+eaf/match-app-p
         (eaf-get-file-name-extension file))
        (eaf-open file))
       (t (apply orig-fn args)))))
  (advice-add #'find-file :around #'+eaf/find-file)
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-image-viewer)
  (with-eval-after-load 'org
    (require 'eaf-org))

  (defun +eaf/match-app-p (extension-name)
    (cl-loop for (app . ext) in eaf-app-extensions-alist
             if (member extension-name (symbol-value ext))
             return app))

  (add-hook 'eaf-mode-hook
            (lambda ()
              (setq left-fringe-width 0
                    right-fringe-width 0)
              (when (featurep 'evil)
                (setq-local evil-motion-state-map nil)
                ;; "C-z" normal-state -> emacs-state
                ;; ":" evil-ex
                (setq-local evil-normal-state-map
                            '(keymap
                              (58 . evil-ex)
                              (26 . evil-emacs-state)))
                ;; evil leader key has higher priority than eaf-mode keys
                (add-to-ordered-list 'emulation-mode-map-alists 'evil-mode-map-alist 1)
                (add-to-ordered-list 'emulation-mode-map-alists 'general-maps-alist 2)
                (add-to-ordered-list 'emulation-mode-map-alists 'eaf--buffer-map-alist 3))))
  ;; Remove unwanted advices
  (advice-remove 'find-file #'eaf--find-file-advisor)
  (advice-remove 'dired-find-file #'eaf--dired-find-file-advisor)
  (advice-remove 'dired-find-alternate-file #'eaf--dired-find-file-advisor)

  (setq eaf-python-command (expand-file-name "~/.pyenv/versions/3.11.1/bin/python")
        eaf-browser-default-search-engine "duckduckgo"
        eaf-config-location (expand-file-name (locate-user-emacs-file "cache/eaf/"))
        eaf-proxy-host personal-proxy-http-host
        eaf-proxy-port (format "%s" personal-proxy-http-port)
        eaf-proxy-type "http"
        eaf-webengine-default-zoom 1.2)

  (defun +eaf/monitor-buffer-kill-a ()
    "A function monitoring when an EAF buffer is killed."
    (run-with-idle-timer
     3 nil
     (lambda (id)
       (ignore-errors
         (eaf-call-async "kill_buffer" id)))
     eaf--buffer-id)
    ;; Kill eaf process when last eaf buffer closed.
    ;; We need add timer to avoid the last web page kill when terminal is exited.
    (run-with-idle-timer
     10 nil
     (lambda ()
       (when (equal (length (eaf--get-eaf-buffers)) 0)
         (eaf--kill-python-process)))))

  (advice-add 'eaf--monitor-buffer-kill :override #'+eaf/monitor-buffer-kill-a))

(use-package eaf-browser
  :load-path "~/.emacs.d/submodules/emacs-application-framework/app/eaf-browser"
  :defer t
  :config
  (setq eaf-browser-blank-page-url "https://duckduckgo.com"
        eaf-browser-dark-mode nil)

  (setq eaf-app-extensions-alist
        (cl-remove-if (lambda (elt) (string-equal "browser" (car elt)))
                      eaf-app-extensions-alist))

  (eaf-bind-key +eaf/switch-to-eww "C-t" eaf-browser-keybinding)
  (eaf-bind-key nil "M-u" eaf-browser-keybinding)
  (eaf-bind-key clear_focus "M-p" eaf-browser-keybinding)
  (eaf-bind-key nil "T" eaf-browser-keybinding)
  (eaf-bind-key recover_prev_close_page "X" eaf-browser-keybinding)

  ;; TODO: evil normal mode clear focus

  (with-eval-after-load 'org
    (setq browse-url-browser-function 'eaf-open-browser))

  (defun +eaf/translate-text (text)
    "Use sdcv to translate selected TEXT."
    ;; (message (format "debug:%s" text))
    (if (featurep 'youdao-dictionary)
        (+eaf/youdao-search text)
      (when (package-installed-p 'youdao-dictionary)
        (require 'youdao-dictionary)
        (+eaf/youdao-search text))))

  (defun +eaf/youdao-search (word)
    "Search WORD simple translate result."
    (let ((result (youdao-dictionary--format-result (youdao-dictionary--request word)))
          (posframe-mouse-banish nil)
          (buf-name youdao-dictionary-buffer-name))
      ;; Show tooltip at point if word fetch from user cursor.
      (posframe-show buf-name
                     :string result
                     :position (if (derived-mode-p 'eaf-mode) (+eaf/mouse-absolute-pixel-position) (point))
                     :timeout 5
                     :internal-border-color (face-foreground 'default)
                     :internal-border-width 1)
      (unwind-protect
          (push (read-event " ") unread-command-events)
        (posframe-delete buf-name))))

  (defun +eaf/mouse-absolute-pixel-position ()
    "Neither `mouse-absolute-pixel-position' or `mouse-pixel-position' work for multi monitors eaf frame,
So I do some dirty hacks for my own user case."
    (let* ((p (mouse-absolute-pixel-position))
           (x (car p))
           (y (cdr p)))
      (if (> x 2560)
          (cons (- x 2560) y)
        p)))

  ;; NOTE: press `Ctrl + <mouse-left-click>` to translate
  (advice-add 'eaf-translate-text :override #'+eaf/translate-text))

(use-package eaf-pdf-viewer
  :load-path "~/.emacs.d/submodules/emacs-application-framework/app/eaf-pdf-viewer"
  :defer t
  :config
  (setq eaf-pdf-dark-mode 'ignore)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-u" eaf-pdf-viewer-keybinding)
  (with-eval-after-load 'evil
    (evil-define-key 'normal eaf-pdf-outline-mode-map (kbd "RET") 'eaf-pdf-outline-jump)))

(use-package eaf-image-viewer
  :load-path "~/.emacs.d/submodules/emacs-application-framework/app/eaf-image-viewer"
  :defer t
  :config
  (setq eaf-image-extension-list '("jpg" "jpeg" "png" "bmp" "gif" "webp")))

(use-package eaf-org
  :load-path "~/.emacs.d/submodules/emacs-application-framework/extension"
  :defer t
  :config
  (defun +org/eaf-open-file (file-path link-without-schema)
    (eaf-open file-path))
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.gif\\'" . +org/eaf-open-file)
                        ("\\.png\\'" . +org/eaf-open-file)
                        ("\\.jpe?g\\'" . +org/eaf-open-file)
                        ("\\.pdf\\'" . +org/eaf-open-file))))


(provide 'init-eaf)

;;; init-eaf.el ends here

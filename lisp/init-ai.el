;; init-ai.el --- AI packages	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  AI packages
;;

;;; Code:

;; A simple ChatGPT client for Emacs
;; https://github.com/karthink/gptel
;; NOTE: https://github.com/karthink/gptel/wiki#defining-custom-gptel-commands
;; NOTE: https://ollama.com/blog/how-to-prompt-code-llama
(use-package gptel
  :defer t
  :bind (:map gptel-mode-map
         ("C-c h" . gptel-menu))
  :config
  (setq gptel-proxy (format "http://%s:%s" personal-proxy-http-host personal-proxy-http-port)
        gptel-expert-commands t
        gptel-log-level 'debug
        ;; I don't want a default system prompt
        gptel--system-message nil
        gptel--rewrite-directive nil)

  (add-hook 'gptel-mode-hook 'turn-on-visual-line-mode)

  ;; Clean Up default backends
  (setq gptel--known-backends nil)

  ;; OpenRouter
  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key 'gptel-api-key
      :models '(anthropic/claude-3-5-haiku
                anthropic/claude-3.5-sonnet
                deepseek/deepseek-r1:free
                google/gemini-2.0-flash-001)))

  ;; DeepSeek
  (defvar gptel--deepseek
    (gptel-make-openai "DeepSeek"
      :host "api.deepseek.com"
      :stream t
      :key 'gptel-api-key
      :models '(deepseek-chat
                deepseek-reasoner)))

  ;; Siliconflow
  (defvar gptel--siliconflow
    (gptel-make-openai "Siliconflow"
      :host "api.siliconflow.cn"
      :stream t
      :key 'gptel-api-key
      :models '(deepseek-ai/DeepSeek-R1
                deepseek-ai/DeepSeek-V3
                Pro/deepseek-ai/DeepSeek-R1
                Pro/deepseek-ai/DeepSeek-V3)))

  ;; VolcEngine
  (defvar gptel--volcengine
    (gptel-make-openai "VolcEngine"
      :host "ark.cn-beijing.volces.com"
      :endpoint "/api/v3/chat/completions"
      :stream t
      :key 'gptel-api-key
      :models '(ep-20250204215608-lx8sx ;; DeepSeek-R1
                ep-20250204215631-zxmvf ;; DeepSeek-V3
                )))

  ;; default model
  (setq gptel-backend gptel--deepseek
        gptel-model 'deepseek-chat))

(use-package gptel-commit
  :ensure nil
  :commands (gptel-commit))

(use-package aidermacs
  :vc (:url "https://github.com/MatthewZMD/aidermacs")
  :bind ("C-c a" . aidermacs-transient-menu)
  :config
  (defun +aider/api-key-from-auth-source (host)
    (when-let* ((auth-info (car (auth-source-search :host host)))
                (secret (plist-get auth-info :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)))
  (setq aidermacs-args '("--model" "openrouter/google/gemini-2.0-flash-001"))
  (setenv "OPENROUTER_API_KEY" (+aider/api-key-from-auth-source "openrouter.ai"))
  (setenv "AIDER_AUTO_COMMITS" "False")
  (setenv "AIDER_CHAT_LANGUAGE" "Chinese")
  (setq aidermacs-backend 'vterm))

(use-package gptel-aibo
  :vc (:url "https://github.com/dolmens/gptel-aibo")
  :defer t)

;; TODO: ai tools
;; - RAG: https://github.com/s-kostyaev/elisa
;; - ai assistant: https://github.com/zbelial/eureka.el
;; - code fim: https://github.com/milanglacier/minuet-ai.el


(provide 'init-ai)

;;; init-ai.el ends here

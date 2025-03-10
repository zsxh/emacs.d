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
        gptel-temperature 0.6
        gptel-include-reasoning 'ignore
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
                anthropic/claude-3.7-sonnet
                deepseek/deepseek-r1:free
                deepseek/deepseek-chat:free
                google/gemini-2.0-flash-lite-001
                google/gemini-2.0-flash-001
                qwen/qwq-32b
                qwen/qwq-32b:free)))

  ;; DeepSeek
  (defvar gptel--deepseek
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key 'gptel-api-key
      :models '(deepseek-chat
                deepseek-reasoner)))

  ;; Siliconflow
  (defvar gptel--siliconflow
    (gptel-make-deepseek "Siliconflow/DeepSeek"
      :host "api.siliconflow.cn"
      :stream t
      :key 'gptel-api-key
      :models '(deepseek-ai/DeepSeek-R1
                deepseek-ai/DeepSeek-V3
                Pro/deepseek-ai/DeepSeek-R1
                Pro/deepseek-ai/DeepSeek-V3
                Qwen/QwQ-32B)))

  ;; VolcEngine
  (defvar gptel--volcengine
    (gptel-make-deepseek "VolcEngine"
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

;; Aider config options, check `https://aider.chat/docs/config/options.html'
(use-package aidermacs
  :bind ("C-c a" . aidermacs-transient-menu)
  :config
  (setq aidermacs-default-model "openrouter/google/gemini-2.0-flash-001"
        aidermacs-editor-model "deepseek/deepseek-chat"
        aidermacs-architect-model "deepseek/deepseek-reasoner")
  (setenv "DEEPSEEK_API_KEY" (auth-source-pick-first-password :host "api.deepseek.com"))
  (setenv "OPENROUTER_API_KEY" (auth-source-pick-first-password :host "openrouter.ai"))
  ;; (setenv "AIDER_AUTO_COMMITS" "False") ;; Disable auto commit of LLM changes
  (setenv "AIDER_CHAT_LANGUAGE" "Chinese") ;; Specify the language to use in the chat
  (setq aidermacs-backend 'vterm))

(use-package gptel-aibo
  :defer t
  :bind ((:map gptel-aibo-complete-mode-map
          ("C-c i" . gptel-aibo-complete-at-point))))

;; TODO: ai tools
;; - RAG: https://github.com/s-kostyaev/elisa
;; - code fim: https://github.com/milanglacier/minuet-ai.el


(provide 'init-ai)

;;; init-ai.el ends here

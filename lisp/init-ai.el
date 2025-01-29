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
        gptel-log-level 'debug)

  (add-hook 'gptel-mode-hook 'turn-on-visual-line-mode)

  ;; Clean Up default backends
  (setq gptel--known-backends nil)

  ;; OpenRouter
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(anthropic/claude-3-5-haiku
              anthropic/claude-3.5-sonnet))

  ;; DeepSeek
  (setq gptel-backend (gptel-make-openai "DeepSeek"
                        :host "api.deepseek.com"
                        :stream t
                        :key 'gptel-api-key
                        :models '(deepseek-chat
                                  deepseek-reasoner)))
  ;; default model
  (setq gptel-model 'deepseek-chat))

(use-package gptel-commit
  :ensure nil
  :commands (gptel-commit))

;; TODO: ai tools
;; - RAG: https://github.com/s-kostyaev/elisa
;; - ai assistant :https://github.com/zbelial/eureka.el
;; - code fim: https://github.com/milanglacier/minuet-ai.el


(provide 'init-ai)

;;; init-ai.el ends here

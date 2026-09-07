(in-package #:llm-backend-llama-cpp/tests)

(deftest-parametrize infer-chat-template
    ((model expected)
     ("/models/Qwen3.5-2B-Q4_K_M.gguf" :chatml)
     ("lmstudio-community/qwen2.5-instruct" :chatml)
     ("Meta-Llama-3.1-8B-Instruct.gguf" :llama3)
     ("llama3-8b-instruct.gguf" :llama3)
     ("/models/fake" :plain)
     (nil :plain))
  (ok (eq expected (llm-backend-llama-cpp:infer-chat-template model))))

(deftest apply-chatml
  (let ((s (llm-backend-llama-cpp:apply-chat-template
            (list (system-turn "Always cite.")
                  (user-turn "hi"))
            :template :chatml)))
    (ok (search "<|im_start|>system" s))
    (ok (search "Always cite.<|im_end|>" s))
    (ok (search "<|im_start|>user" s))
    (ok (search "hi<|im_end|>" s))
    (ok (search "<|im_start|>assistant" s))))

(deftest apply-llama3
  (let ((s (llm-backend-llama-cpp:apply-chat-template
            (list (system-turn "Always cite.")
                  (user-turn "hi"))
            :template :llama3)))
    (ok (search "<|start_header_id|>system<|end_header_id|>" s))
    (ok (search "Always cite.<|eot_id|>" s))
    (ok (search "<|start_header_id|>user<|end_header_id|>" s))
    (ok (search "<|start_header_id|>assistant<|end_header_id|>" s))
    (ng (search "<|begin_of_text|>" s))))

(deftest apply-none-lone-user
  (ok (equal "hi"
             (llm-backend-llama-cpp:apply-chat-template
              (user-turn "hi") :template :none))))

(deftest apply-plain-keeps-labels
  (let ((s (llm-backend-llama-cpp:apply-chat-template
            (list (system-turn "Always cite.")
                  (user-turn "hi"))
            :template :plain)))
    (ok (search "system: Always cite." s))
    (ok (search "user: hi" s))))

(deftest apply-chatml-merges-tools
  (let ((s (llm-backend-llama-cpp:apply-chat-template
            (list (system-turn "Be brief.")
                  (user-turn "add"))
            :template :chatml
            :tools (list (make-llm-tool :name "sum" :description "add numbers")))))
    (ok (search "Available tools" s))
    (ok (search "sum" s))
    (ok (search "Be brief." s))
    (ok (search "<|im_start|>user" s))
    (ok (search "add<|im_end|>" s))))

(deftest apply-rejects-jinja
  (ok (signals (llm-backend-llama-cpp:apply-chat-template
                (user-turn "hi")
                :template "{% for m in messages %}{{ m.content }}{% endfor %}")
               'llm-error)))

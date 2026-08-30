# llm-backend-llama-cpp

[`llm-protocol`](https://github.com/egao1980/llm-protocol) backend over native [`llama-cpp`](https://github.com/egao1980/llama-cpp) (`ggml-org/llama.cpp`). Not HTTP.

`generate` → `llama_stack_complete` (last user text; wave-1 has no tools / stream). `respond` falls back to `generate`. `embed` → `llama_stack_embed` (GGUF families llama.cpp actually loads — `bert`, `qwen3`, …).

```lisp
(asdf:load-system "llm-backend-llama-cpp")
(let ((b (stack-llm-llama-cpp:make-llama-cpp-backend
          :model-path (uiop:getenv "LLAMA_MODEL_PATH"))))
  (stack-llm:embed-query b "ping"))
```

`LLAMA_MODEL_PATH` fills an omitted `:model-path`. Live embed canary is [`cl-stack-llm-demo`](https://github.com/egao1980/cl-stack-llm-demo) `scripts/smoke-embed.lisp`.

Part of [cl-stack](https://github.com/egao1980/cl-stack).

## License

MIT — see [LICENSE](LICENSE).

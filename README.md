# llm-backend-llama-cpp

[`llm-protocol`](https://github.com/egao1980/llm-protocol) backend over native [`llama-cpp`](https://github.com/egao1980/llama-cpp) (`ggml-org/llama.cpp`). Not HTTP.

`generate` → `llama_stack_complete` / `_ex` (last user text; wave-1 has no tools / stream). `respond` falls back to `generate`. `embed` → `llama_stack_embed` (GGUF families llama.cpp actually loads — `bert`, `qwen3`, …).

Structured output is `:output` (JSON Schema / `schema-protocol` designator) → GBNF. Raw GBNF is backend-local via `extra` or `llama-cpp-settings` — not an `llm-protocol` field.

```lisp
(generate b "Oslo" :output schema)   ; schema → GBNF
(generate b "move"
          :settings (stack-llm-llama-cpp:llama-cpp-settings
                     :grammar "root ::= [a-h] [1-8]"))
```

`backend-supports-p` reports `:structured-output` and `:grammar`.

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

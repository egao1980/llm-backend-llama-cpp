(in-package #:llm-backend-llama-cpp)

;;; Known families only. GGUF Jinja (minja / llama-server --jinja) is ABI 5+.
;;; llama_tokenize(..., add_special=true) already prepends BOS — do not emit
;;; <|begin_of_text|> in the Llama-3 template.

(defun %trim-right-nl (s)
  (string-right-trim '(#\Newline #\Return) (or s "")))

(defun %lone-user-p (turns tools)
  (and (null tools)
       (let ((ts (coerce-turns turns)))
         (and ts
              (null (rest ts))
              (eq (llm-turn-role (first ts)) :user)))))

(defun %normalize-chat-template (value)
  (cond
    ((null value) :auto)
    ((or (keywordp value) (stringp value))
     (let ((s (string-downcase (if (keywordp value) (symbol-name value) value))))
       (cond
         ((string= s "auto") :auto)
         ((string= s "none") :none)
         ((string= s "plain") :plain)
         ((string= s "chatml") :chatml)
         ((or (string= s "llama3") (string= s "llama-3")) :llama3)
         ((keywordp value) value)
         (t value))))
    (t (error 'llm-error
              :message (format nil "chat-template must be a keyword or string, got ~s"
                               value)))))

(defun infer-chat-template (model)
  "Guess :chatml / :llama3 / :plain from a GGUF path or model id."
  (let ((path (string-downcase (or model ""))))
    (cond
      ((or (search "llama-3" path) (search "llama3" path)) :llama3)
      ((or (search "qwen" path)
           (search "chatml" path)
           (search "yi-" path)
           (search "internlm" path))
       :chatml)
      (t :plain))))

(defun %assistant-template-text (turn)
  (let ((calls (remove-if-not #'llm-tool-call-part-p (llm-turn-parts turn))))
    (if calls
        (%trim-right-nl
         (with-output-to-string (o)
           (dolist (c calls)
             (format o "{\"name\":~s,\"arguments\":~a}~%"
                     (llm-tool-call-part-name c)
                     (or (llm-tool-call-part-arguments c) "{}")))))
        (or (turn-text turn) ""))))

(defun %tool-template-text (turn)
  (%trim-right-nl
   (with-output-to-string (o)
     (dolist (p (llm-turn-parts turn))
       (when (llm-tool-result-part-p p)
         (format o "tool ~a: ~a~%"
                 (or (llm-tool-result-part-id p) "")
                 (or (llm-tool-result-part-content p) "")))))))

(defun %turn-template-text (turn)
  (ecase (llm-turn-role turn)
    ((:system :user) (or (turn-text turn) ""))
    (:assistant (%assistant-template-text turn))
    (:tool (%tool-template-text turn))))

(defun %template-role (role)
  (ecase role
    (:system "system")
    (:user "user")
    (:assistant "assistant")
    (:tool "user")))

(defun %tools-preamble (tools)
  (when tools
    (with-output-to-string (o)
      (write-line "Available tools. Reply with JSON {\"name\":\"...\",\"arguments\":{...}} to call one, or {\"content\":\"...\"} to answer." o)
      (dolist (tool tools)
        (format o "- ~a~@[: ~a~]~%"
                (llm-tool-name tool)
                (llm-tool-description tool))
        (when (llm-tool-parameters tool)
          (format o "  parameters: ~a~%"
                  (stack-json:encode (llm-tool-parameters tool))))))))

(defun %turns-with-tools (turns tools)
  (let ((ts (coerce-turns turns))
        (preamble (%trim-right-nl (%tools-preamble tools))))
    (if (zerop (length preamble))
        ts
        (if (and ts (eq (llm-turn-role (first ts)) :system))
            (cons (system-turn (format nil "~a~%~a" preamble
                                       (or (turn-text (first ts)) "")))
                  (rest ts))
            (cons (system-turn preamble) ts)))))

(defun %format-plain-prompt (turns)
  (with-output-to-string (o)
    (dolist (turn turns)
      (let ((role (llm-turn-role turn))
            (tx (%turn-template-text turn)))
        (ecase role
          (:system
           (when (plusp (length tx))
             (format o "system: ~a~%" tx)))
          (:user (format o "user: ~a~%" tx))
          (:assistant (format o "assistant: ~a~%" tx))
          (:tool
           (dolist (p (llm-turn-parts turn))
             (when (llm-tool-result-part-p p)
               (format o "tool ~a: ~a~%"
                       (or (llm-tool-result-part-id p) "")
                       (or (llm-tool-result-part-content p) ""))))))))))

(defun %format-chatml-prompt (turns add-generation-prompt)
  (with-output-to-string (o)
    (dolist (turn turns)
      (let ((tx (%turn-template-text turn)))
        (when (or (not (eq (llm-turn-role turn) :system))
                  (plusp (length tx)))
          (format o "<|im_start|>~a~%~a<|im_end|>~%"
                  (%template-role (llm-turn-role turn))
                  tx))))
    (when add-generation-prompt
      (format o "<|im_start|>assistant~%"))))

(defun %format-llama3-prompt (turns add-generation-prompt)
  (with-output-to-string (o)
    (dolist (turn turns)
      (let ((tx (%turn-template-text turn)))
        (when (or (not (eq (llm-turn-role turn) :system))
                  (plusp (length tx)))
          (format o "<|start_header_id|>~a<|end_header_id|>~%~%~a<|eot_id|>"
                  (%template-role (llm-turn-role turn))
                  tx))))
    (when add-generation-prompt
      (format o "<|start_header_id|>assistant<|end_header_id|>~%~%"))))

(defun apply-chat-template (turns &key (template :plain) tools (add-generation-prompt t))
  "Render TURNS with a known family template. TOOLS merge into the system turn.
   TEMPLATE is :none | :plain | :chatml | :llama3. Raw Jinja is not parsed."
  (let ((spec (%normalize-chat-template template)))
    (when (stringp spec)
      (error 'llm-error
             :message (format nil "chat-template Jinja is not supported yet; pass :chatml, :llama3, :plain, :none, or :auto, got ~s"
                              template)))
    (ecase spec
      (:none
       (if (%lone-user-p turns tools)
           (or (turn-text (first (coerce-turns turns))) "")
           (%format-plain-prompt (%turns-with-tools turns tools))))
      (:plain
       (%format-plain-prompt (%turns-with-tools turns tools)))
      (:chatml
       (%format-chatml-prompt (%turns-with-tools turns tools) add-generation-prompt))
      (:llama3
       (%format-llama3-prompt (%turns-with-tools turns tools) add-generation-prompt))
      (:auto
       (error 'llm-error
              :message "apply-chat-template :auto needs infer-chat-template / generate")))))

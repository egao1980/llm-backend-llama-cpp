(in-package #:llm-backend-llama-cpp)

(defvar *complete-fn* #'llama-cpp:complete
  "Injected for tests. (lambda (engine prompt &key max-tokens temperature
    grammar grammar-root on-token) → (values text prompt-tokens completion-tokens)).
   ON-TOKEN is (lambda (piece)); non-NIL return stops.")

(defvar *embed-fn* #'llama-cpp:embed
  "Injected for tests. (lambda (engine texts) → (values vectors dim prompt-tokens)).")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defclass llama-cpp-backend (llm-backend)
  ((model-path :initarg :model-path :accessor llama-cpp-model-path :initform nil)
   (n-ctx :initarg :n-ctx :accessor llama-cpp-n-ctx :initform 2048)
   (engine :initarg :engine :accessor llama-cpp-engine :initform nil)))

(defun make-llama-cpp-backend (&key model-path n-ctx engine)
  (make-instance 'llama-cpp-backend
                 :model-path (or model-path (%env "LLAMA_MODEL_PATH")
                                 (%env "LLAMA_CPP_MODEL"))
                 :n-ctx (or n-ctx 2048)
                 :engine engine))

(defun use-llama-cpp-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-llama-cpp-backend args)))

(defun close-llama-cpp-backend (backend)
  (when (and (llama-cpp-engine backend)
             (llama-cpp:llama-engine-p (llama-cpp-engine backend)))
    (llama-cpp:free-engine (llama-cpp-engine backend))
    (setf (llama-cpp-engine backend) nil))
  backend)

(defun ensure-llama-cpp-engine (backend)
  (or (llama-cpp-engine backend)
      (setf (llama-cpp-engine backend)
            (llama-cpp:load-engine :model-path (llama-cpp-model-path backend)
                                   :n-ctx (llama-cpp-n-ctx backend)))))

(defmethod backend-model ((backend llama-cpp-backend))
  (or (llama-cpp-model-path backend)
      (and (llama-cpp-engine backend)
           (llama-cpp:llama-engine-p (llama-cpp-engine backend))
           (llama-cpp:engine-model-path (llama-cpp-engine backend)))))

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :embeddings)))
  t)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :stream)))
  t)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :tools)))
  t)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :responses)))
  nil)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :grammar)))
  t)

(defmethod backend-supports-p ((backend llama-cpp-backend)
                               (feature (eql :structured-output)))
  t)

(defun %extra-get (extra key)
  (cond
    ((null extra) nil)
    ((hash-table-p extra)
     (or (gethash key extra)
         (gethash (string-downcase (string key)) extra)
         (and (keywordp key) (gethash (symbol-name key) extra))))
    ((consp extra) (getf extra key))
    (t nil)))

(defun %js (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit) (null v))
            do (setf (gethash k h) v))
    h))

(defun %str (x)
  (cond
    ((or (null x) (eq x :null)) "")
    ((stringp x) x)
    (t (princ-to-string x))))

(defun %as-llm-tool (tool)
  (cond
    ((llm-tool-p tool) tool)
    ((and (consp tool) (keywordp (car tool)))
     (make-llm-tool :name (getf tool :name)
                    :description (getf tool :description)
                    :parameters (getf tool :parameters)))
    (t (error 'llm-error :message (format nil "not a tool: ~s" tool)))))

(defun %effective-tools (tools tool-choice)
  (and tools (not (eq tool-choice :none))
       (mapcar #'%as-llm-tool (llm-protocol::%as-list tools))))

(defun %tools-output-schema (tools tool-choice)
  (let* ((names (mapcar #'llm-tool-name tools))
         (tool-obj (%js "type" "object"
                        "properties"
                        (%js "name" (%js "type" "string"
                                         "enum" (coerce names 'vector))
                             "arguments" (%js "type" "object"))
                        "required" #("name" "arguments")
                        "additionalProperties" :false))
         (text-obj (%js "type" "object"
                        "properties" (%js "content" (%js "type" "string"))
                        "required" #("content")
                        "additionalProperties" :false)))
    (if (eq tool-choice :required)
        tool-obj
        (%js "anyOf" (vector tool-obj text-obj)))))

(defun %grammar-from-settings (settings &optional tools tool-choice)
  "→ (values gbnf root). EXTRA :grammar wins over :output schema, then tools."
  (let* ((extra (and settings (llm-settings-extra settings)))
         (raw (%extra-get extra :grammar))
         (root (or (%extra-get extra :grammar-root) "root"))
         (output (and settings (llm-settings-output settings))))
    (cond
      ((and (stringp raw) (plusp (length raw)))
       (values raw root))
      (output
       (values (json-schema-to-gbnf (structured-output-json-schema output))
               root))
      (tools
       (values (json-schema-to-gbnf (%tools-output-schema tools tool-choice))
               "root"))
      (t (values nil nil)))))

(defun %prompt (turns &optional tools)
  (if (null tools)
      (let ((ts (coerce-turns turns)))
        (or (loop for turn in (reverse ts)
                  when (eq (llm-turn-role turn) :user)
                    return (turn-text turn))
            (and ts (turn-text (car (last ts))))
            ""))
      (with-output-to-string (o)
        (write-line "Available tools. Reply with JSON {\"name\":\"...\",\"arguments\":{...}} to call one, or {\"content\":\"...\"} to answer." o)
        (dolist (tool tools)
          (format o "- ~a~@[: ~a~]~%"
                  (llm-tool-name tool)
                  (llm-tool-description tool))
          (when (llm-tool-parameters tool)
            (format o "  parameters: ~a~%"
                    (stack-json:encode (llm-tool-parameters tool)))))
        (terpri o)
        (dolist (turn (coerce-turns turns))
          (ecase (llm-turn-role turn)
            (:system
             (let ((tx (turn-text turn)))
               (when (plusp (length tx))
                 (format o "system: ~a~%" tx))))
            (:user (format o "user: ~a~%" (or (turn-text turn) "")))
            (:assistant
             (let ((calls (remove-if-not #'llm-tool-call-part-p
                                         (llm-turn-parts turn))))
               (if calls
                   (dolist (c calls)
                     (format o "assistant: {\"name\":~s,\"arguments\":~a}~%"
                             (llm-tool-call-part-name c)
                             (or (llm-tool-call-part-arguments c) "{}")))
                   (format o "assistant: ~a~%" (or (turn-text turn) "")))))
            (:tool
             (dolist (p (llm-turn-parts turn))
               (when (llm-tool-result-part-p p)
                 (format o "tool ~a: ~a~%"
                         (or (llm-tool-result-part-id p) "")
                         (or (llm-tool-result-part-content p) ""))))))))))

(defun %parse-complete-text (text tools)
  (if (null tools)
      (values :stop (list (make-llm-text-part :text (or text ""))))
      (let ((obj (ignore-errors
                   (stack-json:decode
                    (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 (or text ""))))))
        (cond
          ((and (hash-table-p obj)
                (let ((name (gethash "name" obj)))
                  (and name (not (eq name :null)) (plusp (length (%str name))))))
           (values :tool-use
                   (list (make-llm-tool-call-part
                          :id "call_0"
                          :name (%str (gethash "name" obj))
                          :arguments (let ((a (gethash "arguments" obj)))
                                       (cond
                                         ((or (null a) (eq a :null)) "{}")
                                         ((stringp a) a)
                                         (t (stack-json:encode a))))))))
          ((and (hash-table-p obj) (nth-value 1 (gethash "content" obj)))
           (values :stop
                   (list (make-llm-text-part :text (%str (gethash "content" obj))))))
          (t (values :stop (list (make-llm-text-part :text (or text "")))))))))

(defun %invoke-complete (backend turns settings &key on-token tools tool-choice)
  (multiple-value-bind (grammar grammar-root)
      (%grammar-from-settings settings tools tool-choice)
    (apply *complete-fn* (ensure-llama-cpp-engine backend) (%prompt turns tools)
           :max-tokens (or (and settings (llm-settings-max-tokens settings)) 32)
           :temperature (or (and settings (llm-settings-temperature settings)) 0.0)
           (append (and on-token (list :on-token on-token))
                   (and grammar
                        (list :grammar grammar :grammar-root grammar-root))))))

(defun %make-complete-response (backend model text pt ct parts &optional finish)
  (make-llm-response
   :parts (or parts (list (make-llm-text-part :text (or text ""))))
   :model (or model (backend-model backend))
   :finish-reason (or finish :stop)
   :usage (make-llm-usage :input-tokens pt :output-tokens ct
                          :total-tokens (and pt ct (+ pt ct)))))

(defmethod generate ((backend llama-cpp-backend) turns &key model settings tools
                     tool-choice output)
  (declare (ignore output))
  (let* ((settings (coerce-settings settings))
         (tools (%effective-tools tools tool-choice)))
    (multiple-value-bind (text pt ct)
        (%invoke-complete backend turns settings
                          :tools tools :tool-choice tool-choice)
      (multiple-value-bind (finish parts)
          (%parse-complete-text text tools)
        (%make-complete-response backend model text pt ct parts finish)))))

(defmethod stream-generate ((backend llama-cpp-backend) turns &key model settings
                            tools tool-choice on-part output)
  (declare (ignore output))
  (let* ((settings (coerce-settings settings))
         (tools (%effective-tools tools tool-choice))
         (parts '()))
    (multiple-value-bind (text pt ct)
        (%invoke-complete backend turns settings
                          :tools tools :tool-choice tool-choice
                          :on-token (lambda (piece)
                                      (let ((part (make-llm-text-part
                                                   :text (or piece ""))))
                                        (push part parts)
                                        (when on-part
                                          (funcall on-part part))
                                        nil)))
      (if tools
          (multiple-value-bind (finish parsed)
              (%parse-complete-text text tools)
            (when (eq finish :tool-use)
              (dolist (p parsed)
                (when on-part (funcall on-part p))))
            (%make-complete-response backend model text pt ct parsed finish))
          (%make-complete-response backend model text pt ct
                                   (and parts (nreverse parts)))))))

(defmethod list-models ((backend llama-cpp-backend) &key)
  (list (make-llm-model-info
         :id (or (backend-model backend) "llama.cpp")
         :owned-by "llama.cpp")))

(defun %slice-embedding (vec dimensions)
  (cond
    ((null dimensions) vec)
    ((> dimensions (length vec))
     (error 'llm-error
            :message (format nil "requested dimensions ~a > model dim ~a"
                             dimensions (length vec))))
    (t (subseq vec 0 dimensions))))

(defmethod embed ((backend llama-cpp-backend) inputs &key model dimensions
                  encoding-format)
  (when (and encoding-format
             (not (member encoding-format '(:float "float") :test #'equal)))
    (error 'llm-unsupported
           :message (format nil "llama.cpp embeddings are float-only, got ~s"
                            encoding-format)))
  (let* ((texts (coerce-embed-inputs inputs))
         (engine (ensure-llama-cpp-engine backend)))
    (multiple-value-bind (vecs dim tokens)
        (funcall *embed-fn* engine texts)
      (declare (ignore dim))
      (make-llm-embed-result
       :embeddings (loop for v in vecs for i from 0
                         collect (make-llm-embedding
                                  :vector (%slice-embedding v dimensions)
                                  :index i))
       :model (or model (backend-model backend))
       :usage (make-llm-usage :input-tokens tokens :total-tokens tokens)))))

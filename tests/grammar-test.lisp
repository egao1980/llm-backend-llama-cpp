(in-package #:llm-backend-llama-cpp/tests)

(defun %js (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(deftest gbnf-object-required
  (let ((g (llm-backend-llama-cpp:json-schema-to-gbnf
            (%js "type" "object"
                 "properties" (%js "name" (%js "type" "string")
                                   "age" (%js "type" "integer"))
                 "required" #("name" "age")
                 "additionalProperties" nil))))
    (ok (search "root ::=" g))
    (ok (search "space ::=" g))
    (ok (search "name" g))
    (ok (search "age" g))
    (ok (search "integral-part ::=" g))))

(deftest gbnf-enum
  (let ((g (llm-backend-llama-cpp:json-schema-to-gbnf
            (%js "enum" #("yes" "no")))))
    (ok (search "yes" g))
    (ok (search "no" g))))

(deftest gbnf-array-of-strings
  (let ((g (llm-backend-llama-cpp:json-schema-to-gbnf
            (%js "type" "array"
                 "items" (%js "type" "string")
                 "minItems" 1))))
    (ok (search "\"[\"" g))
    (ok (search "char" g))))

(deftest gbnf-any-of
  (let ((g (llm-backend-llama-cpp:json-schema-to-gbnf
            (%js "anyOf" (vector (%js "type" "string")
                                 (%js "type" "null"))))))
    (ok (search " | " g))
    (ok (search "null" g))))

(deftest gbnf-ref-defs
  (let ((g (llm-backend-llama-cpp:json-schema-to-gbnf
            (%js "type" "object"
                 "properties" (%js "city" (%js "$ref" "#/$defs/city"))
                 "required" #("city")
                 "$defs" (%js "city" (%js "type" "string"))))))
    (ok (search "city" g))
    (ok (search "char" g))))

(deftest gbnf-rejects-non-hash
  (ok (signals (llm-backend-llama-cpp:json-schema-to-gbnf "nope")
               'llm-error)))

(deftest llama-cpp-settings-extra
  (let ((s (llm-backend-llama-cpp:llama-cpp-settings
            :temperature 0.2
            :grammar "root ::= \"x\""
            :grammar-root "root"
            :extra '(:foo 1))))
    (ok (= 0.2 (llm-settings-temperature s)))
    (ok (equal "root ::= \"x\"" (getf (llm-settings-extra s) :grammar)))
    (ok (equal "root" (getf (llm-settings-extra s) :grammar-root)))
    (ok (eql 1 (getf (llm-settings-extra s) :foo)))))

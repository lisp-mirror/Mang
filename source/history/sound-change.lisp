(in-package #:mang)

;;; .[+obstruent,-voice]1V2/.1[front2]V[round3]2/V3_

(defmethod apply-sound-change ((word word)
                               (sound-change fst))
  (bind ((*phoneme-registry* (make-hash-table :test 'equal))
         (*category-registry* (make-hash-table :test 'equal)))
    (declare (special *phoneme-registry* *category-registry*)
             (type hash-table *phoneme-registry* *category-registry*))
    (image (lambda (solution)
             (word (mapcar #'funcall
                           solution)
                   :origin word
                   :transformations (transformations<- word)))
           (run-fst sound-change (form<- word)))))

;;;; Parser
;;; -> before-write/comp open-registers closed-registers
(defun parse-before (glyphs categories features valued-features open-registers
                     closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          `(,(empty-fst)
            ,open-registers ,closed-registers))
      (>>!
        _ (parse-whitespace)
        (first open-registers closed-registers)
        (parse-compare/write glyphs categories features valued-features
                             open-registers closed-registers)
        _ (parse-whitespace)
        (rest open-registers closed-registers)
        (parse-before glyphs categories features valued-features
                      open-registers closed-registers)
        (succeed `(,(fst-sequence* first rest)
                   ,open-registers ,closed-registers)))))

(defun parse-emitter (glyphs categories features valued-features
                      open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories open-registers valued-features))
  (// (parse-glyphs-emit glyphs)
      (>>!
        category (// (parse-category categories)
                     (parse-constant "."))
        _ (parse-whitespace)
        (constant-features register-features)
        (<? (parse-features-no-write features valued-features
                                     open-registers closed-registers)
            `(,(empty-map)
               ,(empty-map)))
        _ (parse-whitespace)
        register (<? (parse-register))
        (cond
          ((and register (not (@ closed-registers register)))
           (fail `(:register-not-written ,register ,closed-registers)))
          ((and category (not register))
           (fail `(:emit-category-no-register ,category)))
          ((not (or register constant-features register-features))
           (fail `(:empty-emitter)))
          (category
           (succeed
            (fst-emit-category category register constant-features
                               register-features)))
          (register
           (succeed
            (fst-emit-register register constant-features register-features)))
          (t
           (succeed
            (fst-emit constant-features (empty-map)
                      register-features)))))))

;;; -> after-emit
(defun parse-after (glyphs categories features valued-features open-registers
                    closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          (empty-fst))
      (>>!
        first (parse-emitter glyphs categories features valued-features
                             open-registers closed-registers)
        _ (parse-whitespace)
        rest (parse-after glyphs categories features valued-features
                          open-registers closed-registers)
        (succeed (fst-sequence* first rest)))))

(defun parse-sound-change (glyphs categories features valued-features)
  (declare (type set features)
           (type map glyphs categories valued-features))
  (>>!
    _ (parse-whitespace)
    before (parse-to (// (parse-constant "->")
                         (parse-constant "→")))
    _ (parse-whitespace)
    after (parse-to (// (^< (parse-constant "/"))
                        (parse-eof)))
    _ (parse-whitespace)
    (pre post)
    (<? (>>!
          _ (>> (parse-constant "/")
                (parse-whitespace))
          pre (parse-to (parse-constant "_"))
          _ (parse-whitespace)
          post (parse-to (parse-eof))
          (succeed `(,pre ,post)))
        `("" ""))
    _ (parse-expression-end)
    (pre-write/comp pre-emit open-registers closed-registers)
    (^$ (parse-pre/post glyphs categories features valued-features)
        pre)
    (before-write/comp open-registers closed-registers)
    (^$ (parse-before glyphs categories features valued-features open-registers
                      closed-registers)
        before)
    (post-write/comp post-emit open-registers closed-registers)
    (^$ (parse-pre/post glyphs categories features valued-features
                        open-registers closed-registers)
        post)
    after-emit
    (^$ (parse-after glyphs categories features valued-features
                     open-registers closed-registers)
        after)
    (if (and (empty-fst? pre-write/comp)
             (empty-fst? before-write/comp)
             (empty-fst? post-write/comp))
        (fail `(:empty-pre-before-post ,post-write/comp))
        (succeed
         (fst-repeat (fst-preferred (fst-sequence* pre-write/comp
                                                   before-write/comp
                                                   post-write/comp pre-emit
                                                   after-emit post-emit)
                                    (fst-elementary #'true #'list
                                                    :consume? t)))))))

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
(defun parse-glyph-sc (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (parse-whitespace)
    glyph (parse-to (// (^< (parse-constant ","))
                        (^< (parse-constant ")"))))
    ([a]if (@ glyphs glyph)
        (succeed it)
      (fail `(:unknown-glyph ,glyph)))))

(defun parse-glyphs-comp (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "(")
          (parse-whitespace))
    glyph (parse-glyph-sc glyphs)
    glyphs (many (>> (parse-whitespace)
                     (parse-constant ",")
                     (parse-whitespace)
                     (parse-glyph-sc glyphs))
                 (empty-fst)
                 (lambda (glyph glyphs)
                   (fst-sequence* (fst-compare-features glyph)
                                  (fst-consume)
                                  glyphs)))
    _ (>> (parse-whitespace)
          (parse-constant ")"))
    (succeed (fst-sequence* (fst-compare-features glyph)
                            (fst-consume)
                            glyphs))))

(defun parse-glyphs-emit (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "(")
          (parse-whitespace))
    glyph (parse-glyph-sc glyphs)
    glyphs (many (>> (parse-whitespace)
                     (parse-constant ",")
                     (parse-whitespace)
                     (parse-glyph-sc glyphs))
                 (empty-fst)
                 (lambda (glyph glyphs)
                   (fst-sequence* (fst-emit glyph)
                                  glyphs)))
    _ (>> (parse-whitespace)
          (parse-constant ")"))
    (succeed (fst-sequence* (fst-emit glyph)
                            glyphs))))

(defun parse-glyphs-comp-emit (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "(")
          (parse-whitespace))
    glyph (parse-glyph-sc glyphs)
    (comp-glyphs emit-glyphs)
    (many (>> (parse-whitespace)
              (parse-constant ",")
              (parse-whitespace)
              (parse-glyph-sc glyphs))
          `(,(empty-fst) ,(empty-fst))
          (lambda (glyph glyphs)
            `(,(fst-sequence* (fst-compare-features glyph)
                              (fst-consume)
                              glyphs)
              ,(fst-sequence* (fst-emit glyph)
                              glyphs))))
    _ (>> (parse-whitespace)
          (parse-constant ")"))
    (succeed `(,(fst-sequence* (fst-compare-features glyph)
                               (fst-consume)
                               comp-glyphs)
                ,(fst-sequence* (fst-emit glyph)
                                emit-glyphs)))))

(defun parse-register ()
  (// (parse-number)
      (>>!
        _ (>> (parse-whitespace)
              (parse-constant "{")
              (parse-whitespace))
        name (parse-identifier *mang-reserved-symbols*)
        _ (>> (parse-whitespace)
              (parse-constant "}"))
        (succeed name))))

(defun parse-binary-feature (features)
  (>>!
    _ (parse-whitespace)
    sign (// (<$ (parse-constant "+")
                 t)
             (parse-constant "-"))
    _ (parse-whitespace)
    feature (parse-from-set features)
    (succeed `(:binary-feature ,sign ,feature))))

(defun parse-valued-feature (features)
  (>>!
    _ (parse-whitespace)
    feature (parse-from-set (domain features))
    _ (>> (parse-whitespace)
          (parse-constant "=")
          (parse-whitespace))
    value (parse-from-set (@ features feature))
    (succeed `(:valued-feature ,feature ,value))))

(defun parse-register-feature (features)
  (>>!
    _ (parse-whitespace)
    feature (parse-from-set features)
    _ (parse-whitespace)
    register (parse-register)
    (succeed `(:register-feature ,feature ,register))))

(defun parse-feature (features valued-features)
  (// (parse-binary-feature features)
      (parse-valued-feature valued-features)
      (parse-register-feature (union features (domain valued-features)))))

;;; -> constant-features(::map fature value) register-features(::map feature register)
(defun parse-features-no-write (features valued-features open-registers
                                closed-registers)
  (declare (type set features closed-registers)
           (type map valued-features))
  (labels ((_inner-parser ()
             (>>!
               feature (parse-feature features valued-features)
               _ (parse-whitespace)
               (constant-features register-features)
               (<? (>> (parse-constant ",")
                       (parse-whitespace)
                       (_inner-parser))
                   `(,(empty-map)
                      ,(empty-map)))
               (bind (((type &rest args)
                       feature))
                 (ecase type
                   (:binary-feature
                    (bind (((sign feature)
                            args))
                      (succeed
                       `(,(with constant-features feature sign)
                          ,register-features))))
                   (:valued-feature
                    (bind (((feature value)
                            args))
                      (succeed
                       `(,(with constant-features feature value)
                          ,register-features))))
                   (:register-feature
                    (bind (((feature register)
                            args))
                      (if (or (@ closed-registers register)
                              (@ (@ open-registers register)
                                 feature))
                          (succeed
                           `(,constant-features
                             ,(with register-features feature register)))
                          (fail `(:compare-unwritten-feature ,register
                                                             ,feature))))))))))
    (>>!
      _ (>> (parse-whitespace)
            (parse-constant "[")
            (parse-whitespace))
      features (_inner-parser)
      _ (>> (parse-whitespace)
            (parse-constant "]"))
      (succeed features))))

(defun parse-features (features valued-features open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map valued-features open-registers))
  (labels ((_inner-parser (open-registers)
             (>>!
               feature (parse-feature features valued-features)
               (constant-feature register-feature-compare register-feature-write
                                 open-registers)
               (succeed
                (bind (((type &rest args)
                        feature))
                  (ecase type
                    (:binary-feature
                     (bind (((sign feature)
                             args))
                       `(,(map (feature sign))
                          ,(empty-map)
                          ,(empty-map)
                          ,open-registers)))
                    (:valued-feature
                     (bind (((feature value)
                             args))
                       `(,(map (feature value))
                          ,(empty-map)
                          ,(empty-map)
                          ,open-registers)))
                    (:register-feature
                     (bind (((feature register)
                             args))
                       (if (@ closed-registers register)
                           `(,(empty-map)
                              ,(map (feature register))
                              ,(empty-map)
                              ,open-registers)
                           ([av]if (@ open-registers register)
                               `(,(empty-map)
                                  ,(map (feature register))
                                  ,(empty-map)
                                  ,(with open-registers register
                                         (with it feature)))
                             `(,(empty-map)
                                ,(empty-map)
                                ,(map (feature register))
                                ,(with open-registers register
                                       (set feature))))))))))
               _ (parse-whitespace)
               (constant-features register-features-compare
                                  register-features-write open-registers)
               (<? (>> (parse-constant ",")
                       (parse-whitespace)
                       (_inner-parser open-registers))
                   `(,(empty-map)
                      ,(empty-map)
                      ,(empty-map)
                      ,open-registers))
               (succeed `(,(map-union constant-feature constant-features)
                           ,(map-union register-feature-compare
                                       register-features-compare)
                           ,(map-union register-feature-write
                                       register-features-write)
                           ,open-registers)))))
    (>>!
      _ (>> (parse-whitespace)
            (parse-constant "[")
            (parse-whitespace))
      features (_inner-parser open-registers)
      _ (>> (parse-whitespace)
            (parse-constant "]"))
      (succeed features))))

(defun parse-compare/write-emit (glyphs categories features valued-features
                                 open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$> (parse-glyphs-comp-emit glyphs)
           (lambda (action)
             `(,@action ,open-registers ,closed-registers)))
      (<$> (>>!
             category (// (parse-category categories)
                          (parse-constant "."))
             _ (parse-whitespace)
             (constant-features register-features-compare
                                register-features-write open-registers)
             (<? (parse-features features valued-features open-registers
                                 closed-registers)
                 `(,(empty-map)
                    ,(empty-map)
                    ,(empty-map)
                    ,open-registers))
             _ (parse-whitespace)
             register (<? (parse-register)
                          (gensym))
             (succeed
              `(,(fst-sequence*
                  (fst-write-features register-features-write)
                  (fst-compare-register register-features-compare)
                  (fst-compare-features constant-features)
                  (cond
                    ((and category (@ closed-registers register))
                     (fst-sequence* (fst-compare-register register)
                                    (fst-check-category category)))
                     (category
                      ([av]if (@ open-registers register)
                          (fst-sequence* (fst-compare-features it)
                                         (fst-write-category category register))
                        (fst-write-category category register)))
                     ((@ closed-registers register)
                      (fst-compare-register register))
                     (t
                      ([av]if (@ open-registers register)
                          (fst-sequence* (fst-compare-features it)
                                         (fst-write-register register))
                        (fst-write-register register)))))
                 ,(fst-emit-register register)
                 ,(less open-registers register)
                 ,(with closed-registers register))))
           (lambda (action)
             (bind (((comp/write-action
                      emit-action
                      &optional
                      (open-registers (empty-map (empty-set)))
                      (closed-registers (empty-set)))
                     action))
               `(,(fst-sequence* comp/write-action (fst-consume))
                  ,emit-action ,open-registers ,closed-registers))))))

;;; -> pre-write/comp pre-emit open-registers closed-registers
(defun parse-pre/post (glyphs categories features valued-features
                       &optional (open-registers (empty-map (empty-set)))
                              (closed-registers (empty-set)))
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          `(,(empty-fst)
             ,(empty-fst)
             ,open-registers ,closed-registers))
      (>>!
        (first-write/comp first-emit open-registers closed-registers)
        (parse-compare/write-emit glyphs categories features valued-features
                                  open-registers closed-registers)
        _ (parse-whitespace)
        (rest-write/comp rest-emit open-registers closed-registers)
        (parse-pre/post glyphs categories features valued-features
                        open-registers closed-registers)
        (succeed `(,(fst-sequence* first-write/comp rest-write/comp)
                    ,(fst-sequence* first-emit rest-emit)
                    ,open-registers ,closed-registers)))))

(defun parse-compare/write (glyphs categories features valued-features
                            open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$> (parse-glyphs-comp glyphs)
           (lambda (action)
             `(,action ,open-registers ,closed-registers)))
      (<$> (>>!
             category (// (parse-category categories)
                          (parse-constant "."))
             _ (parse-whitespace)
             (constant-features register-features-compare
                                register-features-write open-registers)
             (<? (parse-features features valued-features open-registers
                                 closed-registers)
                 `(,(empty-map)
                    ,(empty-map)
                    ,(empty-map)
                    ,open-registers))
             _ (parse-whitespace)
             register (<? (parse-register))
             (succeed
              `(,(fst-sequence*
                  (fst-compare-features constant-features)
                  (fst-write-features register-features-write)
                  (fst-compare-register register-features-compare)
                  (cond
                    ((and category register (@ closed-registers register))
                     (fst-sequence* (fst-compare-register register)
                                    (fst-check-category category)))
                    ((and category register)
                     ([av]if (@ open-registers register)
                         (fst-sequence* (fst-compare-features it)
                                        (fst-write-category category
                                                            register))
                       (fst-write-category category register)))
                    (category
                     (fst-check-category category))
                    ((and register (@ closed-registers register))
                     (fst-compare-register register))
                    (register
                     ([av]if (@ open-registers register)
                         (fst-sequence* (fst-compare-features it)
                                        (fst-write-register register))
                       (fst-write-register register)))
                    (t (empty-fst))))
                 ,(less open-registers register)
                 ,(if register
                      (with closed-registers register)
                      closed-registers))))
           (lambda (action)
             (bind (((action open-registers closed-registers)
                     action))
               `(,(fst-sequence* action (fst-consume))
                 ,open-registers ,closed-registers))))))

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

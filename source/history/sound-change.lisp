(in-package #:mang)

;;; .[+obstruent,-voice]1V2/.1[front2]V[round3]2/V3_

(defun has-features? (phoneme features)
  (declare (type map phoneme features))
  (not (@ (gmap :set (lambda (feature value)
                       (eql (@ phoneme feature)
                            value))
                (:map features))
          nil)))

(defmethod in-category? ((phoneme map)
                         (category sequence))
  (position-if (lambda (features)
                 (has-features? phoneme features))
               category))

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
(defun parse-glyph (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (parse-constant "<")
    category (parse-from-map glyphs)
    _ (parse-constant ">")
    (succeed category)))

(defun parse-category (categories)
  (declare (type map categories))
  (parse-from-map categories))

(defun parse-register ()
  (// (parse-number)
      (>>!
        _ (parse-constant "{")
        _ (parse-whitespace)
        name (parse-identifier)
        _ (parse-whitespace)
        _ (parse-constant "}")
        (succeed name))))

(defun parse-binary-feature (features)
  (>>!
    sign (// (<$ t (parse-constant "+"))
             (parse-constant "-"))
    feature (parse-from-set features)
    (succeed `(:binary-feature ,sign ,feature))))

(defun parse-valued-feature (features)
  (>>!
    feature (parse-from-set (domain features))
    _ (parse-constant "<")
    value (parse-from-set (@ features feature))
    _ (parse-constant ">")
    (succeed `(:valued-feature ,feature ,value))))

(defun parse-register-feature (features)
  (>>!
    feature (parse-from-set features)
    _ (parse-whitespace)
    register (parse-register)
    (succeed `(:register-feature ,feature ,register))))

(defun parse-feature (features valued-features)
  (// (parse-binary-feature features)
      (parse-valued-feature valued-features)
      (parse-register-feature (union features (domain valued-features)))))

;;;; -> constant-features(::map fature value) register-features(::map feature register)
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
      _ (parse-constant "[")
      _ (parse-whitespace)
      features (_inner-parser)
      _ (parse-whitespace)
      _ (parse-constant "]")
      (succeed features))))

(defun parse-features (features valued-features open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map valued-features open-registers))
  (labels ((_inner-parser (open-registers)
             (>>!
               feature (parse-feature features valued-features)
               (constant-feature register-feature open-registers)
               (succeed
                (bind (((type &rest args)
                        feature))
                  (ecase type
                    (:binary-feature
                     (bind (((sign feature)
                             args))
                       `(,(map (feature sign))
                          ,(empty-map)
                          ,open-registers)))
                    (:valued-feature
                     (bind (((feature value)
                             args))
                       `(,(map (feature value))
                          ,(empty-map)
                          ,open-registers)))
                    (:register-feature
                     (bind (((feature register)
                             args))
                       `(,(empty-map)
                         ,(map (feature register))
                          ,(if (@ closed-registers register)
                               open-registers
                               (with open-registers register
                                     (with (@ open-registers register)
                                           feature)))))))))
               _ (parse-whitespace)
               (constant-features register-features open-registers)
               (<? (>> (parse-constant ",")
                       (parse-whitespace)
                       (_inner-parser open-registers))
                   `(,(empty-map)
                      ,(empty-map)
                      ,open-registers))
               (succeed `(,(map-union constant-feature constant-features)
                           ,(map-union register-feature register-features)
                           ,open-registers)))))
    (>>!
      _ (parse-constant "[")
      _ (parse-whitespace)
      features (_inner-parser open-registers)
      _ (parse-whitespace)
      _ (parse-constant "]")
      (succeed features))))

(defun parse-compare/write-emit (glyphs categories features valued-features
                                 open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (<$> (lambda (action)
         (bind (((comp/write-action emit-action)
                 action))
           `((:sequence ,comp/write-action (:consume))
             ,emit-action)))
       (// (<$> (lambda (glyph)
                  `((:compare ,glyph)
                    (:emit ,glyph)))
                (parse-glyph glyphs))
           (>>!
             category (// (parse-category categories)
                          (parse-constant "."))
             _ (parse-whitespace)
             (constant-features register-features open-registers
                                closed-registers)
             (<? (parse-features features valued-features open-registers
                                 closed-registers)
                 `(,(empty-map)
                    ,(empty-map)
                    ,open-registers ,closed-registers))
             _ (parse-whitespace)
             register (<? (parse-register)
                          (gensym))
             (succeed
              `((:sequence
                 (:compare-features (:load-features ,register-features))
                 (:sequence
                  (:compare-features ,constant-features)
                  ,(cond
                     ((and category (@ register closed-registers))
                      `(:sequence (:compare ,register)
                                  (:check-category ,category)))
                     (category
                      ([av]if (@ register open-registers)
                          `(:sequence (:check-features ,it)
                                      (:write-category ,register))
                        `(:write-category ,category ,register)))
                     ((@ register closed-registers)
                      `(:compare ,register))
                     (t
                      ([av]if (@ register open-registers)
                          `(:sequence (:compare-features ,it)
                                      (:write ,register))
                        `(:write ,register))))))
                (:emit ,register)
                ,(less open-registers register)
                ,(with closed-registers register)))))))

;;; -> pre-write/comp pre-emit open-registers closed-registers
(defun parse-pre/post (glyphs categories features valued-features
                       &optional (open-registers (empty-map (empty-set)))
                              (closed-registers (empty-set)))
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (<? (>>!
        (first-write/comp first-emit)
        (parse-compare/write-emit glyphs categories features valued-features
                                  open-registers closed-registers)
        _ (parse-whitespace)
        (rest-write/comp rest-emit)
        (parse-pre/post glyphs categories features valued-features
                        open-registers closed-registers)
        (succeed `((:sequence ,first-write/comp ,rest-write/comp)
                   (:sequence ,first-emit ,rest-emit))))
      `(:empty)))

(defun parse-compare/write (glyphs categories features valued-features
                            open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (<$> (lambda (action)
         (bind (((action open-registers closed-registers)
                 action))
           `((:sequence ,action (:consume))
             ,open-registers ,closed-registers)))
       (// (<$> (lambda (glyph)
                  `((:compare-features ,glyph)
                    ,open-registers ,closed-registers))
                (parse-glyph glyphs))
           (>>!
             category (// (parse-category categories)
                          (parse-constant "."))
             _ (parse-whitespace)
             (constant-features register-features open-registers
                                closed-registers)
             (<? (parse-features features valued-features open-registers
                                 closed-registers)
                 `(,(empty-map)
                    ,(empty-map)
                    ,open-registers ,closed-registers))
             _ (parse-whitespace)
             register (parse-register)
             (succeed
              `((:sequence
                 (:compare-features ,constant-features)
                 (:sequence
                  (:compare-features (:load-features ,register-features))
                  ,(cond
                     ((and category register (@ closed-registers register))
                      `(:sequence (:compare ,register)
                                  (:check-category ,category)))
                     ((and category register)
                      ([av]if (@ open-registers register)
                          ;; `:write-category` has to include a check for the
                          ;; category
                          `(:sequence (:compare-features ,it)
                                      (:write-category ,category
                                                       ,register))))
                     (category
                      `(:check-category ,category))
                     ((and register (@ closed-registers register))
                      `(:compare ,register))
                     (register
                      ([av]if (@ open-registers register)
                          `(:sequence (:compare-features ,it)
                                      (:write ,register))
                        `(:write ,register)))
                     (t `(:empty)))))
                ,(less open-registers register)
                ,(if register
                     (with closed-registers register)
                     closed-registers)))))))

;;; -> before-write/comp open-registers closed-registers
(defun parse-before (glyphs categories features valued-features open-registers
                     closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (<? (>>!
        first (parse-compare/write glyphs categories features valued-features
                                   open-registers closed-registers)
        _ (parse-whitespace)
        rest (parse-before glyphs categories features valued-features
                           open-registers closed-registers)
        (succeed `(:sequence ,first ,rest)))
      `(:empty)))

(defun parse-emitter (glyphs categories features valued-features
                      open-registers closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories open-registers valued-features))
  (// (parse-glyph glyphs)
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
            `(:emit
              (:supplement (:load-category ,category ,register)
                           (:supplement ,constant-features
                                        (:load-features ,register-features))))))
          (register
           (succeed
            `(:emit
              (:supplement (:load-register ,register)
                           (:supplement ,constant-features
                                        (:load-features ,register-features))))))
          (t
           (succeed
            `(:emit (:supplement ,constant-features
                                 (:load-features ,register-features)))))))))

;;; -> after-emit
(defun parse-after (glyphs categories features valued-features open-registers
                    closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (<? (>>!
        first (parse-emitter glyphs categories features valued-features
                             open-registers closed-registers)
        _ (parse-whitespace)
        rest (parse-after glyphs categories features valued-features
                          open-registers closed-registers)
        (succeed `(:sequence ,first ,rest)))
      `(:empty)))

(defun parse-sound-change (glyphs categories features valued-features)
  (declare (type set features)
           (type map glyphs categories valued-features))
  (>>!
    before (parse-to (// (parse-constant "->")
                         (parse-constant "→")))
    _ (parse-whitespace)
    after (parse-to (// (^< (parse-constant "/"))
                        (parse-eof)))
    _ (parse-whitespace)
    (pre post)
    (<? (>>!
          _ (parse-constant "/")
          _ (parse-whitespace)
          pre (parse-to (parse-constant "_"))
          _ (parse-whitespace)
          post (parse-to (parse-eof))
          (succeed `(,pre ,post)))
        `((:empty) (:empty)))
    _ (parse-whitespace)
    _ (parse-eof)
    (pre-write/comp pre-emit open-registers closed-registers)
    (^$ (parse-pre/post glyphs categories features valued-features)
        pre)
    (before-write/comp open-registers closed-registers)
    (^$ (parse-before glyphs categories features valued-features open-registers
                      closed-registers)
        before)
    (post-write/comp post-emit closed-registers)
    (^$ (parse-pre/post glyphs categories features valued-features
                        open-registers closed-registers)
        post)
    after-emit
    (^$ (parse-after glyphs categories features valued-features
                     open-registers closed-registers)
        after)
    (succeed `(:sequence ,pre-write/comp ,before-write/comp ,post-write/comp
                         ,pre-emit ,after-emit ,post-emit))))

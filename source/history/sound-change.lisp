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
(defparameter *sound-change-reserved-symbols*
   (set #\( #\) #\{ #\} #\[ #\] #\< #\> #\. #\, #\+ #\- #\→ #\= #\:))

(defun parse-expression-end ()
  (// (parse-newline)
      (parse-eof)))

(defun parse-glyph (glyphs)
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
    glyph (parse-glyph glyphs)
    glyphs (many (>> (parse-whitespace)
                     (parse-constant ",")
                     (parse-whitespace)
                     (parse-glyph glyphs))
                 `(:empty)
                 (lambda (glyph glyphs)
                   `(:sequence (:sequence (:compare-features ,glyph)
                                          (:consume))
                               ,glyphs)))
    _ (>> (parse-whitespace)
          (parse-constant ")"))
    (succeed `(:sequence (:sequence (:compare-features ,glyph)
                                    (:consume))
                         ,glyphs))))

(defun parse-glyphs-emit (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "(")
          (parse-whitespace))
    glyph (parse-glyph glyphs)
    glyphs (many (>> (parse-whitespace)
                     (parse-constant ",")
                     (parse-whitespace)
                     (parse-glyph glyphs))
                 `(:empty)
                 (lambda (glyph glyphs)
                   `(:sequence (:emit ,glyph)
                               ,glyphs)))
    _ (>> (parse-whitespace)
          (parse-constant ")"))
    (succeed `(:sequence (:emit ,glyph)
                         ,glyphs))))

(defun parse-glyphs-comp-emit (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "(")
          (parse-whitespace))
    glyph (parse-glyph glyphs)
    (comp-glyphs emit-glyphs)
    (many (>> (parse-whitespace)
              (parse-constant ",")
              (parse-whitespace)
              (parse-glyph glyphs))
          `((:empty) (:empty))
          (lambda (glyph glyphs)
            `((:sequence (:sequence (:compare-features ,glyph)
                                    (:consume))
                         ,glyphs)
              (:sequence (:emit ,glyph)
                         ,glyphs))))
    _ (>> (parse-whitespace)
          (parse-constant ")"))
    (succeed `((:sequence (:sequence (:compare-features ,glyph)
                                     (:consume))
                          ,comp-glyphs)
               (:sequence (:emit ,glyph)
                          ,emit-glyphs)))))

(defun parse-category (categories)
  (declare (type map categories))
  (// (>>!
        _ (>> (parse-whitespace)
              (parse-constant "<")
              (parse-whitespace))
        name (parse-identifier *sound-change-reserved-symbols*)
        _ (>> (parse-whitespace)
              (parse-constant ">"))
        ([av]if (@ categories name)
            (succeed it)
          `(:unknown-category ,name)))
      (>>* (parse-unicode-property "Letter")
           name
           ([av]if (@ categories name)
               (succeed it)
             (fail `(:unknown-category ,name))))))

(defun parse-register ()
  (// (parse-number)
      (>>!
        _ (>> (parse-whitespace)
              (parse-constant "{")
              (parse-whitespace))
        name (parse-identifier *sound-change-reserved-symbols*)
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
                          ,open-registers))
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
                                   (set feature)))))))))
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
  (<$> (// (parse-glyphs-comp-emit glyphs)
           (>>!
             category (// (parse-category categories)
                          (parse-constant "."))
             _ (parse-whitespace)
             (constant-features register-features-compare
                                 register-features-write closed-registers)
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
                 (:sequence
                  (:write-features ,register-features-write)
                  (:compare-features
                   (:load-features ,register-features-compare)))
                 (:sequence
                  (:compare-features ,constant-features)
                  ,(cond
                     ((and category (@ closed-registers register))
                      `(:sequence (:compare ,register)
                                  (:check-category ,category)))
                     (category
                      ([av]if (@ open-registers register)
                          `(:sequence (:check-features ,it)
                                      (:write-category ,register))
                        `(:write-category ,category ,register)))
                     ((@ closed-registers register)
                      `(:compare ,register))
                     (t
                      ([av]if (@ open-registers register)
                          `(:sequence (:compare-features ,it)
                                      (:write ,register))
                        `(:write ,register))))))
                (:emit ,register)
                ,(less open-registers register)
                ,(with closed-registers register)))))
       (lambda (action)
         (bind (((comp/write-action
                  emit-action
                  &optional
                  (open-registers (empty-map (empty-set)))
                  (closed-registers (empty-set)))
                 action))
           `((:sequence ,comp/write-action (:consume))
             ,emit-action ,open-registers ,closed-registers)))))

;;; -> pre-write/comp pre-emit open-registers closed-registers
(defun parse-pre/post (glyphs categories features valued-features
                       &optional (open-registers (empty-map (empty-set)))
                              (closed-registers (empty-set)))
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          `((:empty)
            (:empty)
            ,open-registers ,closed-registers))
      (>>!
        (first-write/comp first-emit open-registers closed-registers)
        (parse-compare/write-emit glyphs categories features valued-features
                                  open-registers closed-registers)
        _ (parse-whitespace)
        (rest-write/comp rest-emit open-registers closed-registers)
        (parse-pre/post glyphs categories features valued-features
                        open-registers closed-registers)
        (succeed `((:sequence ,first-write/comp ,rest-write/comp)
                   (:sequence ,first-emit ,rest-emit)
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
                    ,open-registers))
             _ (parse-whitespace)
             register (<? (parse-register))
             (succeed
              `((:sequence
                 (:compare-features ,constant-features)
                 (:sequence
                  (:sequence
                   (:write-features ,register-features-write)
                   (:compare-features
                    (:load-features ,register-features-compare)))
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
                                                       ,register))
                        `(:write-category ,category ,register)))
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
                     closed-registers))))
           (lambda (action)
             (bind (((action open-registers closed-registers)
                     action))
               `((:sequence ,action (:consume))
                 ,open-registers ,closed-registers))))))

;;; -> before-write/comp open-registers closed-registers
(defun parse-before (glyphs categories features valued-features open-registers
                     closed-registers)
  (declare (type set features closed-registers)
           (type map glyphs categories valued-features open-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          `((:empty)
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
        (succeed `((:sequence ,first ,rest)
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

;;;; Supporting parsers
(defun parse-binary-feature-definition ()
  (>>!
    name (parse-identifier *sound-change-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":")
          (parse-whitespace)
          (parse-constant "binary")
          (parse-expression-end))
    (succeed name)))

(defun parse-valued-feature-definition ()
  (>>!
    name (parse-identifier *sound-change-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":")
          (parse-whitespace)
          (parse-constant "valued")
          (parse-whitespace)
          (parse-constant "=")
          (parse-whitespace))
    value (parse-identifier *sound-change-reserved-symbols*)
    values (some (>> (parse-whitespace)
                     (parse-constant ",")
                     (parse-whitespace)
                     (parse-identifier *sound-change-reserved-symbols*))
                 (empty-set)
                 (lambda (value values)
                   (with values value)))
    _ (parse-expression-end)
    (succeed `(,name ,(with values value)))))

(defun parse-feature-definitions ()
  (many (>> (parse-whitespace)
            (// (parse-binary-feature-definition)
                (parse-valued-feature-definition)))
        `(,(empty-set)
           ,(empty-map (empty-set)))
        (lambda (feature features)
          (bind (((constant-features valued-features)
                  features))
            (if (consp feature)
                (bind (((name values)
                        feature))
                  `(,constant-features ,(with valued-features name values)))
                `(,(with constant-features feature)
                   ,valued-features))))))

(defun parse-glyph-definition (features valued-features)
  (>>!
    _ (parse-whitespace)
    name (parse-identifier *sound-change-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant "=")
          (parse-whitespace))
    (feature value)
    (// (>>!
          sign (// (<$ (parse-constant "+")
                       t)
                   (<$ (parse-constant "-")
                       nil))
          _ (parse-whitespace)
          feature (parse-identifier *sound-change-reserved-symbols*) 
          (if (@ features feature)
              (succeed `(,feature ,sign))
              (fail `(:unknown-binary-feature ,feature ,sign))))
        (>>!
          feature (parse-identifier *sound-change-reserved-symbols*)
          _ (>> (parse-whitespace)
                (parse-constant "<")
                (parse-whitespace))
          value (parse-identifier *sound-change-reserved-symbols*)
          _ (>> (parse-whitespace)
                (parse-constant ">"))
          ([a]if (@ valued-features feature)
              (if (@ it value)
                  (succeed `(,feature ,value))
                  (fail `(:unknown-feature-value ,feature ,value)))
            (fail `(:unknown-valued-feature ,feature ,value)))))
    features
    (many (>> (parse-whitespace)
              (parse-constant ",")
              (parse-whitespace)
              (// (>>!
                    sign (// (<$ (parse-constant "+")
                                 t)
                             (<$ (parse-constant "-")
                                 nil))
                    _ (parse-whitespace)
                    feature (parse-identifier *sound-change-reserved-symbols*)
                    (if (@ features feature)
                        (succeed `(,feature ,sign))
                        (fail `(:unknown-binary-feature ,feature ,sign))))
                  (>>!
                    feature (parse-identifier *sound-change-reserved-symbols*)
                    _ (>> (parse-whitespace)
                          (parse-constant "<")
                          (parse-whitespace))
                    value (parse-identifier *sound-change-reserved-symbols*)
                    _ (>> (parse-whitespace)
                          (parse-constant ">"))
                    ([a]if (@ valued-features feature)
                        (if (@ it value)
                            (succeed `(,feature ,value))
                            (fail `(:unknown-feature-value ,feature ,value)))
                      (fail `(:unknown-valued-feature ,feature ,value))))))
          (empty-map)
          (lambda (feature features)
            (bind (((name value)
                    feature))
              (with features name value))))
    _ (parse-expression-end)
    (succeed `(,name ,(with features feature value)))))

(defun parse-glyph-definitions (features valued-features)
  (many (>> (parse-whitespace)
            (parse-glyph-definition features valued-features))
        (empty-map (empty-set))
        (lambda (glyph-def glyph-defs)
          (bind (((name features)
                  glyph-def))
            (with glyph-defs name features)))))

(defun parse-category-definition (glyphs)
  (>>!
    _ (parse-whitespace)
    name (parse-identifier *sound-change-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant "=")
          (parse-whitespace))
    glyph (>>!
            name (parse-identifier *sound-change-reserved-symbols*)
            ([av]if (@ glyphs name)
                (succeed it)
              (fail `(:unknown-glyph ,name))))
    glyphs (many (>>!
                   _ (>> (parse-whitespace)
                         (parse-constant ",")
                         (parse-whitespace))
                   name (parse-identifier *sound-change-reserved-symbols*)
                   ([av]if (@ glyphs name)
                       (succeed it)
                     (fail `(:unknown-glyph ,name))))
                 '() #'cons)
    _ (parse-expression-end)
    (succeed `(,name (,glyph ,@glyphs)))))

(defun parse-category-definitions (glyphs)
  (many (parse-category-definition glyphs)
        (empty-map (empty-map))
        (lambda (category categories)
          (bind (((name contents)
                  category))
            (with categories name contents)))))

(defun parse-phonology ()
  (>>!
    (binary-features valued-features)
    (parse-feature-definitions)
    glyphs (parse-glyph-definitions binary-features valued-features)
    categories (parse-category-definitions glyphs)
    (succeed `(,binary-features ,valued-features ,glyphs ,categories))))

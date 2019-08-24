(in-package #:mang)

(defun fst-filter (predicate)
  (declare (type function predicate))
  (fst-elementary predicate '()
                  :consume? nil))

(defun fst-emit (generator)
  (declare (type function generator))
  (fst-elementary #'true generator
                  :consume? nil))

(defun fst-compare-phoneme (phoneme)
  (declare (type map phoneme))
  (fst-filter (lambda (other-phoneme)
                (declare (type map other-phoneme))
                (equal? phoneme other-phoneme?))))

(defun fst-compare-features (constant present absent)
  (declare (type map constant)
           (type set present absent))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (has-features? phoneme constant present absent))))

(defun fst-compare-register (register)
  (declare (special *phoneme-registry*))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (has-features? phoneme (gethash register *phoneme-registry*
                                                (empty-map))
                               (empty-set)
                               (empty-set)))))

(defun fst-emit-phoneme (phoneme
                         &key
                           (constant-supplement (empty-map))
                           (present-supplement (empty-set))
                           (absent-supplement (empty-set))
                           (register-supplement (empty-map)))
  (declare (special *phoneme-registry*)
           (type map phoneme constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-emit
   (lambda (glyph)
     (declare (ignore glyph))
     (list (augment-feature-set
            phoneme
            (map-union constant-supplement
                       (image (lambda (feature register)
                                (values feature
                                        (@ (gethash register *phoneme-registry*)
                                           feature)))
                              register-supplement))
            present-supplement absent-supplement)))))

(defun fst-emit-register (register
                          &key
                            (constant-supplement (empty-map))
                            (present-supplement (empty-set))
                            (absent-supplement (empty-set))
                            (register-supplement (empty-map)))
  (declare (special *phoneme-registry*)
           (type map constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-emit
   (lambda (glyph)
     (declare (ignore glyph))
     (list (augment-feature-set
            (gethash register *phoneme-registry*)
            (map-union constant-supplement
                       (image (lambda (feature register)
                                (values feature
                                        (@ (gethash register *phoneme-registry*)
                                           feature)))
                              register-supplement))
            present-supplement absent-supplement)))))

(defun fst-emit-category (category register
                          &key
                            (constant-supplement (empty-map))
                            (present-supplement (empty-set))
                            (absent-supplement (empty-set))
                            (register-supplement (empty-map)))
  (declare (special *category-registry* *phoneme-registry*)
           (type map category constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-elementary
   #'true
   (lambda (glyph)
     (declare (ignore glyph))
     (list
      (map-union (nth (gethash register *category-registry*)
                      category)
                 (map-union constant-supplement
                            (image (lambda (feature register)
                                     (values feature
                                             (@ (gethash register
                                                         *phoneme-registry*)
                                                feature)))
                                   register-supplement)))))
   :consume? nil))

(defun fst-write-features (features)
  (declare (special *phoneme-registry*)
           (type map features))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (dolist (feature (convert 'list features)
                         t)
                  (bind (((feature register)
                          feature))
                    (setf (gethash register *phoneme-registry*)
                          (with (gethash register *phoneme-registry*)
                                feature (@ phoneme feature))))))))
/
(defun fst-write-register (register)
  (declare (special *phoneme-registry*))
  (fst-elementary (lambda (phoneme)
                    (declare (type map phoneme))
                    (setf (gethash register *phoneme-registry*)
                          phoneme)
                    t)))

(defun fst-write-category (category register)
  (declare (special *category-registry* *phoneme-registry*)
           (type map category))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                ([a]when (in-category? phoneme category)
                  (setf (gethash register *category-registry*)
                        it
                        (gethash register *phoneme-registry*)
                        phoneme)
                  t))))

(defun check-category (category)
  (declare (type category))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (in-category? phoneme category))))

(defun parse-glyphs-comp (glyphs)
  (declare (type map glyphs))
  (some (>> (parse-whitespace-no-newline)
            (parse-glyph glyphs))
        (empty-fst)
        (lambda (glyph filter)
          (bind (((_ phoneme)
                  glyph))
            (fst-sequence (fst-compare-phoneme phoneme)
                          filter)))))

(defun parse-glyphs-emit (glyphs)
  (declare (type map glyphs))
  (some (>> (parse-whitespace-no-newline)
            (parse-glyph glyphs))
        (empty-fst)
        (lambda (glyph emitter)
          (bind (((_ phoneme)
                  glyph))
            (fst-sequence (fst-emit-phoneme phoneme)
                          emitter)))))

(defun parse-glyphs-comp-emit (glyphs)
  (declare (type map glyph))
  (some (>> (parse-whitespace-no-newline)
            (parse-glyph glyphs))
        `(,(empty-fst)
           ,(empty-fst))
        (lambda (glyph filter/emitter)
          (bind (((_ phoneme)
                  glyph)
                 ((filter emitter)
                  filter/emitter))
            `(,(fst-sequence (fst-compare-phoneme phoneme)
                             filter)
               ,(fst-sequence (fst-emit-phoneme phoneme)
                              emitter))))))

(defun parse-register ()
  (// (parse-number)
      (parse-wrapped "{" (parse-identifier *mang-reserved-symbols*)
                     "}")))

(defun parse-register-feature (binary-features valued-features
                               privative-features)
  (declare (type set binary-features valued-features)
           (type map privative-features))
  (>>!
    feature (parse-from-set (union (union binary-features privative-features)
                                   (domain valued-features)))
    register (parse-register)
    `(,feature ,register)))

(defun parse-features-emit (binary-features valued-features privative-features
                            feature-registers phoneme-registers
                            category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features feature-registers))
  (parse-wrapped
   "["
   (parse-separated (// (<$> (parse-feature-spec binary-features valued-features
                                                 privative-features)
                             (lambda (feature)
                               (bind (((valued present _)
                                       feature))
                                 `(,(map-union (convert 'set present
                                                        :key-fn #'identity
                                                        :value-fn
                                                        (constantly t))
                                               valued)
                                    ,(empty-map)))))
                        (>>!
                          (feature register)
                          (parse-register-feature binary-features
                                                     valued-features
                                                     privative-features)
                          (if (or (@ phoneme-registers register)
                                  (@ category-registers register)
                                  (@ (@ feature-registers register)
                                     feature))
                              (succeed `(,(empty-map)
                                          ,(map (feature register))))
                              (fail `(:compare-unwritten-feature ,register
                                                                 ,feature)))))
                    ","
                    `(,(empty-map)
                       ,(empty-map))
                    (lambda (feature features)
                      (bind (((constant register)
                              feature)
                             ((constants registers)
                              features))
                        `(,(map-union constants constant)
                           ,(map-union registers register)))))
   "]"))

(defun parse-features (binary-features valued-features privative-features
                       feature-registers phoneme-registers category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features feature-registers))
  (parse-wrapped
   "["
   (parse-separated
    (// (<$> (parse-feature-spec binary-features valued-features
                                 (union (union binary-features
                                               privative-features)
                                        (domain valued-features)))
             (lambda (feature)
               (bind (((valued present absent)
                       feature))
                 `(,valued ,present ,absent ,(empty-map)
                           ,(empty-map)
                           ,(empty-map (empty-set))))))
        (<$> (parse-register-feature binary-features valued-features
                                     privative-features)
             (lambda (feature)
               (bind (((feature register)
                       feature))
                 (if (@ (@ feature-registers register)
                        feature)
                     `(,(empty-map)
                        ,(empty-set)
                        ,(empty-set)
                        ,(map (feature register))
                        ,(empty-map)
                        ,(empty-map (empty-set)))
                     `(,(empty-map)
                        ,(empty-set)
                        ,(empty-set)
                        ,(empty-map)
                        ,(map (feature register))
                        ,(map (register (set feature))
                              :default (empty-set))))))))
    ","
    `(,(empty-map)
       ,(empty-set)
       ,(empty-set)
       ,(empty-map)
       ,(empty-map)
       ,(empty-map (empty-set)))
    (lambda (feature features)
      (bind (((constant present absent comp-register write-register
                        feature-register)
              feature)
             ((constants presents absents comp-registers write-registers
                         feature-registers)
              features))
        `(,(map-union constants constant)
           ,(union presents present)
           ,(union absents absent)
           ,(map-union comp-registers comp-register)
           ,(map-union write-registers write-register)
           ,(map-union feauture-registers feature-register
                       #'union)))))
   "]"))

(defun parse-comp/write-emit (binary-features valued-features privative-features
                              glyphs categories feature-registers
                              phoneme-registers category-registers)
  (// (>>!
        category (// (parse-category categories)
                     (parse-constant "."))
        _ (parse-whitespace-no-newline)
        (constant-features present-features absent-features
                           comp-feature-registers write-feature-registers
                           feature-registers)
        (<? (parse-features binary-features valued-features privative-features
                            feature-registers phoneme-registers
                            category-registers)
            `(,(empty-map)
               ,(empty-set)
               ,(empty-set)
               ,(empty-map)
               ,(empty-map (empty-set))))
        _ (parse-whitespace-no-newline)
        register (<? (parse-register)
                     (gensym))
        (succeed
         `(,(fst-sequence*
             (fst-compare-features constant-features present-features
                                   absent-features)
             (fst-compare-register comp-feature-registers)
             (fst-write-features write-feature-registers)
             (cond
               ((and category (@ category-registers register))
                (fst-sequence (fst-check-category category)
                              (fst-compare-register register)))
               ((and category (@ phoneme-registers register))
                (fst-sequence (fst-compare-register register)
                              (fst-write-category category register)))
               (category
                ([av]if (@ feature-registers register)
                    (fst-sequence (fst-compare-features it)
                                  (fst-write-category category register))
                  (fst-write-category category register)))
               ((or (@ phoneme-registers register)
                    (@ category-registers register))
                (fst-compare-register register))
               (t
                ([av]if (@ feature-registers register)
                    (fst-sequence (fst-compare-features it)
                                  (fst-write-register register))
                  (fst-write-register register)))))
            ,(less feature-registers register)
            ,(if category
                 (less phoneme-registers register)
                 phoneme-registers)
            ,(if category
                 (with category-registers register)
                 category-registers))))
      (<$> (parse-glyphs-comp-emit glyphs)
           (lambda (action)
             `(,@action ,feature-registers ,phoneme-registers
                        ,category-registers)))))

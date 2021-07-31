(in-package #:mang)

(defun fst-filter (predicate)
  (declare (type function predicate))
  (fst-elementary predicate #'empty
                  :consume? t
                  :in-state (gensym "fst-filter-in")
                  :out-state (gensym "fst-filter-out")))

(defun fst-emit (generator)
  (declare (type function generator))
  (fst-elementary #'true generator
                  :consume? nil
                  :in-state (gensym "fst-emit-in")
                  :out-state (gensym "fst-emit-out")))

(defun fst-compare-phoneme (phoneme)
  (declare (type map phoneme))
  (fst-filter (lambda (other-phoneme)
                (declare (type map other-phoneme))
                (equal? phoneme other-phoneme))))

(defun fst-compare-features (constant &optional
                                        (present (empty-set))
                                        (absent (empty-set)))
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
           (type list category)
           (type map constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-emit
   (lambda (glyph)
     (declare (ignore glyph))
     (list
      (augment-feature-set
       (nth (gethash register *category-registry*)
            category)
       (map-union constant-supplement
                  (image (lambda (feature register)
                           (values feature
                                   (@ (gethash register
                                               *phoneme-registry*)
                                      feature)))
                         register-supplement))
       present-supplement absent-supplement)))))

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

(defun fst-write-register (register)
  (declare (special *phoneme-registry*))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (setf (gethash register *phoneme-registry*)
                      phoneme)
                t)))

(defun fst-write-category (category register)
  (declare (special *category-registry* *phoneme-registry*)
           (type sequence category))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                ([a]when (in-category? phoneme category)
                  (setf (gethash register *category-registry*)
                        it
                        (gethash register *phoneme-registry*)
                        phoneme)
                  t))))

(defun fst-check-category (category)
  (declare (type list category))
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
                          emitter
                          :in-state (gensym "glyphs-emit-in")
                          :out-state (gensym "glyphs-emit-out"))))))

(defun parse-glyphs-comp-emit (glyphs)
  (declare (type map glyphs))
  (some (parse-glyph glyphs)
        `(,(empty-fst)
           ,(empty-fst))
        (lambda (glyph filter/emitter)
          (bind (((_ phoneme)
                  glyph)
                 ((filter emitter)
                  filter/emitter))
            `(,(fst-sequence (fst-compare-phoneme phoneme)
                             filter
                             :in-state (gensym "glyphs-comp-emit-emit-in")
                             :out-state (gensym "glyphs-com-emit-emit-out"))
              ,(fst-sequence (fst-emit-phoneme phoneme)
                             emitter
                             :in-state (gensym "glyphs-comp-emit-comp-in")
                             :out-state
                             (gensym "glyphs-comp-emit-comp-out")))))))

(defun parse-register ()
  (// (parse-number)
      (parse-wrapped "{" (parse-identifier *mang-reserved-symbols*)
                     "}")))

(defun parse-register-feature (binary-features valued-features
                               privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
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
                               (bind (((constant present absent)
                                       feature))
                                 `(,constant ,present ,absent ,(empty-map)))))
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
                                          ,(empty-set)
                                          ,(empty-set)
                                          ,(map (feature register))))
                              (fail `(:compare-unwritten-feature ,register
                                                                 ,feature)))))
                    ","
                    `(,(empty-map)
                       ,(empty-set)
                       ,(empty-set)
                       ,(empty-map))
                    (lambda (feature features)
                      (bind (((constant present absent register)
                              feature)
                             ((constants presents absents registers)
                              features))
                        `(,(map-union constants constant)
                           ,(union presents present)
                           ,(union absents absent)
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
                 (if (or (@ phoneme-registers register)
                         (@ category-registers register)
                         (@ (@ feature-registers register)
                            feature))
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
           ,(map-union feature-registers feature-register
                       #'union)))))
   "]"))

(defun parse-comp/write-emit (binary-features valued-features privative-features
                              glyphs categories feature-registers
                              phoneme-registers category-registers)
  (// (>>!
        (_ category)
        (// (parse-category categories)
            (<$ (parse-constant ".")
                (list nil (convert 'list (range glyphs)))))
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
                              (fst-compare-register register)
                              :in-state
                              (gensym "parse-comp/write-emit-cc-cr-in")
                              :out-state
                              (gensym "parse-comp/write-emit-cc-cr-out")))
               ((and category (@ phoneme-registers register))
                (fst-sequence (fst-compare-register register)
                              (fst-write-category category register)
                              :in-state
                              (gensym "parse-comp/write-emit-cr-wc-in")
                              :out-state
                              (gensym "parse-comp/write-emit-cr-wc-out")))
               (category
                ([av]if (@ feature-registers register)
                    (fst-sequence (fst-compare-features it)
                                  (fst-write-category category register)
                                  :in-state
                                  (gensym "parse-comp/write-emit-cf-wc-in")
                                  :out-state
                                  (gensym "parse-comp-write-emit-cf-wc-out"))
                  (fst-write-category category register)))
               ((or (@ phoneme-registers register)
                    (@ category-registers register))
                (fst-compare-register register))
               (t
                ([av]if (@ feature-registers register)
                    (fst-sequence (fst-compare-features it)
                                  (fst-write-register register)
                                  :in-state
                                  (gensym "parse-comp/write-emit-cf-wr-in")
                                  :out-state
                                  (gensym "parse-comp/write-emit-cf-wr-out"))
                  (fst-write-register register)))))
            ,(fst-emit-register register)
            ,(less feature-registers register)
            ,(if category
                 (less phoneme-registers register)
                 (with phoneme-registers register))
            ,(if category
                 (with category-registers register)
                 category-registers))))
      (<$> (parse-glyphs-comp-emit glyphs)
           (lambda (action)
             `(,@action ,feature-registers ,phoneme-registers
                        ,category-registers)))))

(defun parse-pre/post (binary-features valued-features privative-features glyphs
                       categories feature-registers phoneme-registers
                       category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features glyphs categories feature-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          `(,(empty-fst)
             ,(empty-fst)
             , feature-registers ,phoneme-registers ,category-registers))
      (>>!
        (comp/write emit feature-registers phoneme-registers category-registers)
        (parse-comp/write-emit binary-features valued-features
                               privative-features glyphs categories
                               feature-registers phoneme-registers
                               category-registers)
        _ (parse-whitespace-no-newline)
        (comp/writes emits feature-registers phoneme-registers
                     category-registers)
        (parse-pre/post binary-features valued-features privative-features
                        glyphs categories feature-registers phoneme-registers
                        category-registers)
        (succeed `(,(fst-sequence comp/write comp/writes
                                  :in-state
                                  (gensym "parse-pre/post-comp/write-in")
                                  :out-state
                                  (gensym "parse-pre/post-comp/write-out"))
                    ,(fst-sequence emit emits
                                   :in-state (gensym "parse-pre/post-emit-in")
                                   :out-state
                                   (gensym "parse-pre/post-emit-out"))
                    ,feature-registers ,phoneme-registers
                    ,category-registers)))))

(defun parse-comp/write (binary-features valued-features privative-features
                         glyphs categories feature-registers
                         phoneme-registers category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features glyphs categories feature-registers))
  (// (<$> (>>!
             category (// (parse-category categories)
                          (parse-constant "."))
             _ (parse-whitespace-no-newline)
             (constant-features present-features absent-features
                                comp-feature-registers write-feature-registers
                                feature-registers)
             (<? (parse-features binary-features valued-features
                                 privative-features feature-registers
                                 phoneme-registers category-registers)
                 `(,(empty-map)
                    ,(empty-set)
                    ,(empty-set)
                    ,(empty-map)
                    ,(empty-map)
                    ,(empty-map (empty-set))))
             _ (parse-whitespace-no-newline)
             register (<? (parse-register))
             (succeed
              `(,(fst-sequence*
                  (fst-compare-features constant-features present-features
                                        absent-features)
                  (fst-compare-register comp-feature-registers)
                  (fst-write-features write-feature-registers)
                  (cond
                    ((and category register (@ category-registers register))
                     (fst-sequence (fst-check-category category)
                                   (fst-compare-register register)
                                   :in-state
                                   (gensym "parse-comp/write-cc-cr-in")
                                   :out-state
                                   (gensym "parse-comp/write-cc-cr-out")))
                    ((and category register (@ phoneme-registers register))
                     (fst-sequence (fst-compare-register register)
                                   (fst-write-category category register)
                                   :in-state
                                   (gensym "parse-comp/write-cr-wc-in")
                                   :out-state
                                   (gensym "parse-comp/write-cr-wc-out")))
                    ((and category register)
                     ([av]if (@ feature-registers register)
                         (fst-sequence (fst-compare-features it)
                                       (fst-write-category category register)
                                       :in-state
                                       (gensym "parse-comp/write-cf-wc-in")
                                       :out-state
                                       (gensym "parse-comp/write-cf-wc-out"))
                       (fst-write-category category register)))
                    (category
                     (fst-check-category category))
                    ((and register (or (@ phoneme-registers register)
                                       (@ category-registers register)))
                     (fst-compare-register register))
                    (register
                     ([av]if (@ feature-registers register)
                         (fst-sequence (fst-compare-features it)
                                       (fst-write-register register)
                                       :in-state
                                       (gensym "parse-comp/write-cf-wr-in")
                                       :out-state
                                       (gensym "parse-comp/write-cf-wr-out"))
                       (fst-write-register register)))
                    (t (empty-fst))))
                 ,(if register
                      (less feature-registers register)
                      feature-registers)
                 ,(if (or category (not register))
                      (less phoneme-registers register)
                      (with phoneme-registers register))
                 ,(if (and category register)
                      (with category-registers register)
                      category-registers))))
           (lambda (action)
             (bind (((action feature-registers phoneme-registers
                             category-registers)
                     action))
               `(,(fst-sequence action (fst-consume)
                                :in-state (gensym "parse-comp/write-result-in")
                                :out-state
                                (gensym "parse-comp/write-result-out"))
                  ,feature-registers ,phoneme-registers ,category-registers))))
      (<$> (parse-glyphs-comp glyphs)
           (lambda (action)
             `(,action ,feature-registers ,phoneme-registers
                       ,category-registers)))))

(defun parse-before (binary-features valued-features privative-features glyphs
                     categories feature-registers phoneme-registers
                     category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features glyphs categories feature-registers))
  (// (<$ (>> (parse-whitespace)
              (parse-eof))
          `(,(empty-fst)
             ,feature-registers ,phoneme-registers ,category-registers))
      (>>!
        (first feature-registers phoneme-registers category-registers)
        (parse-comp/write binary-features valued-features privative-features
                          glyphs categories feature-registers phoneme-registers
                          category-registers)
        _ (parse-whitespace-no-newline)
        (rest feature-registers phoneme-registers category-registers)
        (parse-before binary-features valued-features privative-features glyphs
                      categories feature-registers phoneme-registers
                      category-registers)
        (succeed `(,(fst-sequence first rest
                                  :in-state (gensym "parse-before-in")
                                  :out-state (gensym "parse-before-out"))
                    ,feature-registers ,phoneme-registers
                    ,category-registers)))))

(defun parse-emitter (binary-features valued-features privative-features glyphs
                      categories feature-registers phoneme-registers
                      category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features glyphs categories feature-registers))
  (// (>>!
        category (// (parse-category categories)
                     (parse-constant "."))
        _ (parse-whitespace-no-newline)
        (constant-features present-features absent-features register-features)
        (<? (parse-features-emit binary-features valued-features
                                 privative-features feature-registers
                                 phoneme-registers category-registers)
            `(,(empty-map)
               ,(empty-set)
               ,(empty-set)
               ,(empty-map)))
        _ (parse-whitespace-no-newline)
        register (<? (parse-register))
        (cond
          ((and register (not (or (@ phoneme-registers register)
                                  (@ category-registers register))))
           (fail `(:register-not-written ,register
                                         ,(union phoneme-registers
                                                 category-registers))))
          ((and category (not register))
           (fail `(:emit-category-no-register ,category)))
          ((and category (not (@ category-registers register)))
           (fail `(:emit-category-unwritten ,category ,register)))
          ((and (not register)
                (empty? constant-features)
                (empty? register-features))
           (fail `(:empty-emitter)))
          (category
           (succeed (fst-emit-category category register
                                       :constant-supplement constant-features
                                       :present-supplement present-features
                                       :absent-supplement absent-features
                                       :register-supplement register-features)))
          (register
           (succeed (fst-emit-register register
                                       :constant-supplement constant-features
                                       :present-supplement present-features
                                       :absent-supplement absent-features
                                       :register-supplement register-features)))
          (t
           (succeed (fst-emit-phoneme constant-features
                                      :present-supplement present-features
                                      :absent-supplement absent-features
                                      :register-supplement
                                      register-features)))))
      (parse-glyphs-emit glyphs)))

(defun parse-after (binary-features valued-features privative-features glyphs
                    categories feature-registers phoneme-registers
                    category-registers)
  (declare (type set binary-features privative-features phoneme-registers
                 category-registers)
           (type map valued-features glyphs categories feature-registers))
  (<? (>>!
        _ (parse-whitespace-no-newline)
        first (parse-emitter binary-features valued-features privative-features
                             glyphs categories feature-registers
                             phoneme-registers category-registers)
        rest (parse-after binary-features valued-features privative-features
                          glyphs categories feature-registers phoneme-registers
                          category-registers)
        (succeed (fst-sequence first rest
                               :in-state (gensym "parse-after-in")
                               :out-state (gensym "parse-after-out"))))
      (empty-fst)))

(defun parse-sound-change (binary-features valued-features privative-features
                           glyphs categories)
  (declare (type set binary-features privative-features)
           (type map valued-features glyphs categories))
  (>>!
    before (parse-to (>> (parse-whitespace-no-newline)
                         (// (parse-constant "->")
                             (parse-constant "→"))))
    _ (parse-whitespace-no-newline)
    after (parse-to (>> (parse-whitespace-no-newline)
                        (^< (// (parse-constant "/")
                                (parse-expression-end)))))
    _ (parse-whitespace-no-newline)
    (pre post)
    (<? (>>!
          _ (>> (parse-whitespace-no-newline)
                (parse-constant "/")
                (parse-whitespace-no-newline))
          pre (parse-to (>> (parse-whitespace-no-newline)
                            (parse-constant "_")))
          _ (parse-whitespace-no-newline)
          post (parse-to (>> (parse-whitespace-no-newline)
                             ;; this parser should not consume the newline after
                             ;; the sound change expression
                             (^< (parse-expression-end))))
          (succeed `(,pre ,post)))
        `("" ""))
    (pre-write/comp pre-emit feature-registers phoneme-registers
                    category-registers)
    (^$ (parse-pre/post binary-features valued-features privative-features
                        glyphs categories (empty-map (empty-set))
                        (empty-set)
                        (empty-set))
        pre)
    (before-write/comp feature-registers phoneme-registers category-registers)
    (^$ (parse-before binary-features valued-features privative-features glyphs
                      categories feature-registers phoneme-registers
                      category-registers)
        before)
    (post-write/comp post-emit feature-registers phoneme-registers
                     category-registers)
    (^$ (parse-pre/post binary-features valued-features privative-features
                        glyphs categories feature-registers phoneme-registers
                        category-registers)
        post)
    after-emit
    (^$ (parse-after binary-features valued-features privative-features glyphs
                     categories feature-registers phoneme-registers
                     category-registers)
        after)
    (if (and (empty-fst? pre-write/comp)
             (empty-fst? before-write/comp)
             (empty-fst? post-write/comp))
        (fail `(:empty-pre/before/post ,after-emit))
        (succeed (fst-repeat
                  (fst-preferred
                   (fst-sequence* pre-write/comp before-write/comp
                                  post-write/comp pre-emit after-emit post-emit)
                   (fst-elementary #'true #'list
                                   :consume? t
                                   :in-state
                                   (gensym "parse-sound-change-in")
                                   :out-state
                                   (gensym "parse-sound-change-out"))))))))

(defun parse-sound-change-section (binary-features valued-features
                                   privative-features glyphs categories)
  (parse-section "sound changes"
                 (parse-lines (parse-sound-change binary-features
                                                  valued-features
                                                  privative-features
                                                  glyphs categories))))

(defun apply-sound-change (word sound-change)
  (declare (type cons word)
           (type fst sound-change))
  (bind ((*category-registry* (make-hash-table :test 'equal))
         (*phoneme-registry* (make-hash-table :test 'equal)))
    (declare (special *category-registry* *phoneme-registry*)
             (type hash-table *category-registry* *phoneme-registry*))
    (run-fst sound-change word)))

(defun apply-sound-changes (word &rest sound-changes)
  (reduce-nd #'apply-sound-change
             sound-changes word))

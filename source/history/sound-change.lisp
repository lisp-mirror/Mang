(in-package #:mang)

;;; .[+obstruent,-voice]1V2/.1[front2]V[round3]2/V3_

(defun has-features? (phoneme features)
  (declare (type map phoneme features))
  (@ (gmap :set (lambda (feature value)
                  (eql (@ phoneme feature)
                       value))
           (:map features))
     t))

(defmethod in-category? ((phoneme map)
                         (category sequence))
  (position-if (lambda (features)
                 (has-features? phoneme features))
               category))

(defun load-glyph (&key
                     phoneme-register phoneme
                     category-register category
                     (features-from-register (empty-map))
                     (constant-features (empty-map)))
  (declare (special *registry*)
           (type symbol phoneme-register category-register)
           (type sequence category)
           (type map features-from-register constant-features)
           (type (or map null)
                 phoneme))
  (assert (if category-register
              category
              (not category)))
  (assert (if phoneme-register
              (not phoneme)
              phoneme))
  (bind ((phoneme (or phoneme (gethash phoneme-register *registry*))))
    (map-union
     (map-union
      (if category
          (@ category (gethash category-register *registry*))
          phoneme)
      (image (lambda (feature register)
               (values feature (@ (gethash register *registry*)
                                  feature)))
             features-from-register))
     constant-features)))

(defmethod apply-sound-change ((word word)
                               (sound-change fst))
  (bind ((*registry* (make-hash-table :test 'eq)))
    (declare (special *registry*)
             (type hash-table *registry*))
    (image (lambda (solution)
             (word (mapcar #'funcall
                           solution)
                   :origin word
                   :transformations (transformations<- word)))
           (run-fst sound-change (form<- word)))))

;;;; Parser
(defun parse-category (categories)
  (declare (type set categories))
  (// (>>!
        _ (parse-constant "<")
        name (parse-identifier)
        _ (parse-constant ">")
        (succeed name))
      (parse-prefix-set categories)))

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
    feature (parse-prefix-set features)
    (succeed `(:binary-feature ,sign ,feature))))

(defun parse-valued-feature (features)
  (>>!
    feature (parse-prefix-set (domain features))
    _ (parse-constant "<")
    value (parse-prefix-set (@ features feature))
    _ (parse-constant ">")
    (succeed `(:valued-feature ,feature ,value))))

(defun parse-register-feature (features)
  (>>!
    feature (parse-prefix-set features)
    _ (parse-whitespace)
    register (parse-register)
    (succeed `(:register-feature ,register ,feature))))

(defun parse-registerless-feature (features valued-features)
  (// (parse-binary-feature features)
      (parse-valued-feature valued-features)))

(defun parse-feature (features valued-features)
  (// (parse-binary-feature features)
      (parse-valued-feature valued-features)
      (parse-register-feature (union features (domain valued-features)))))

(defun parse-features-no-write (features valued-features closed-registers)
  (declare (type set features closed-registers)
           (type map valued-features))
  (todo features valued-features closed-registers))

;;; -> pre-write/comp pre-emit open-registers closed-registers
(defun parse-pre (glyphs categories features valued-features category-map)
  (declare (type set glyphs categories features)
           (type map valued-features category-map))
  (todo glyphs categories features valued-features category-map))

;;; -> before-write/comp open-registers closed-registers
(defun parse-before (glyphs categories features valued-features category-map
                     open-registers closed-registers)
  (declare (type set glyphs categories features closed-registers)
           (type map valued-features category-map open-registers))
  (todo glyphs categories features valued-features category-map
        open-registers closed-registers))

;;; -> post-write/comp post-emit closed-registes
(defun parse-post (glyphs categories features valued-features category-map
                   open-registers closed-registers)
  (declare (type set glyphs categories features closed-registers)
           (type map valued-features category-map open-registers))
  (todo glyphs categories features valued-features category-map
        open-registers closed-registers))

(defun parse-emitter (glyphs categories features valued-features category-map
                      closed-registers)
  (declare (type set glyphs categories features closed-registers)
           (type map valued-features category-map))
  (// (<$> (lambda (glyph)
             `(:emit-constant ,glyph))
           (parse-prefix-set glyphs))
      (>>!
        category (// (parse-category categories)
                     (parse-constant "."))
        _ (parse-whitespace)
        features (<? (parse-features-no-write features valued-features
                                              closed-registers))
        _ (parse-whitespace)
        register (<? (parse-register))
        (if (@ closed-registers register)
            (if features
                (if register
                    (todo category category-map)
                    (todo category category-map))
                (if register
                    (todo category category-map)
                    (todo category category-map)))
            (fail `(:register-not-written ,register ,closed-registers))))))

;;; -> after-emit
(defun parse-after (glyphs categories features valued-features category-map
                    closed-registers)
  (declare (type set glyphs categories features closed-registers)
           (type map valued-features category-map))
  (<? (>>!
        first (parse-emitter glyphs categories features valued-features
                             category-map closed-registers)
        _ (parse-whitespace)
        rest (parse-after glyphs categories features valued-features
                          category-map closed-registers)
        (succeed `(:sequence ,first ,rest)))
      `(:empty)))

(defun parse-sound-change (glyphs categories features valued-features category-map)
  (declare (type set glyphs categories features)
           (type map valued-features category-map))
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
    (^$ (parse-pre glyphs categories features valued-features category-map)
        pre)
    (before-write/comp open-registers closed-registers)
    (^$ (parse-before glyphs categories features valued-features category-map
                      open-registers closed-registers)
        before)
    (post-write/comp post-emit closed-registers)
    (^$ (parse-post glyphs categories features valued-features category-map
                    open-registers closed-registers)
        post)
    after-emit
    (^$ (parse-after glyphs categories features valued-features category-map
                     closed-registers)
        after)
    (succeed `(:sequence ,pre-write/comp ,before-write/comp ,post-write/comp
                         ,pre-emit ,after-emit ,post-emit))))

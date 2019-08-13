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

(defun parse-feature-set (features valued-features)
  (>>!
    _ (parse-constant "[")
    _ (parse-whitespace)
    feature (parse-feature features valued-features)
    features (many (>> (parse-whitespace)
                       (parse-constant ",")
                       (parse-whitespace)
                       (parse-feature features valued-features))
                   (empty-set)
                   (lambda (val set)
                     (with set val)))
    _ (parse-whitespace)
    _ (parse-constant "]")
    (succeed (with features feature))))

(defun parse-registerless-feature-set (features valued-features)
  (>>!
    _ (parse-constant "[")
    _ (parse-whitespace)
    feature (parse-registerless-feature features valued-features)
    features (many (>> (parse-whitespace)
                       (parse-constant ",")
                       (parse-whitespace)
                       (parse-registerless-feature features valued-features))
                   (empty-set)
                   (lambda (val set)
                     (with set val)))
    _ (parse-whitespace)
    _ (parse-constant "]")
    (succeed (with features feature))))

;;; -> pre-write/comp pre-emit open-registers closed-registers
(defun parse-pre (categories features valued-features category-map)
  (declare (type set categories features)
           (type map valued-features category-map))
  (todo categories features valued-features category-map))

;;; -> before-write/comp open-registers closed-registers
(defun parse-before (categories features valued-features category-map
                     open-registers closed-registers)
  (declare (type set categories features closed-registers)
           (type map valued-features category-map open-registers))
  (todo categories features valued-features category-map
        open-registers closed-registers))

;;; -> post-write/comp post-emit closed-registes
(defun parse-post (categories features valued-features category-map
                   open-registers closed-registers)
  (declare (type set categories features closed-registers)
           (type map valued-features category-map open-registers))
  (todo categories features valued-features category-map
        open-registers closed-registers))

(defun parse-emitter (categories features valued-features category-map
                      closed-registers)
  (declare (type set categories features closed-registers)
           (type map valued-features category-map))
  (>>!
    category (parse-category categories)
    features (<? (parse-feature-set features valued-features))
    register (<? (parse-register))
    (if (@ closed-registers register)
        (succeed
         (todo category category-map features register))
        (fail `(:register-not-written ,register ,closed-registers)))))

;;; -> after-emit
(defun parse-after (categories features valued-features category-map
                    closed-registers)
  (declare (type set categories features closed-registers)
           (type map valued-features category-map))
  (<? (>>!
        first (parse-emitter categories features valued-features category-map
                             closed-registers)
        rest (parse-after categories features valued-features category-map
                          closed-registers)
        (succeed `(:sequence ,first ,rest)))
      `(:empty)))

(defun parse-sound-change (categories features valued-features category-map)
  (declare (type set categories features)
           (type map valued-features category-map))
  (>>!
    before (parse-to (// (parse-constant "->")
                         (parse-constant "→")))
    after (parse-to (// (parse-constant "/")
                        (parse-eof)))
    pre (<? (parse-to (parse-constant "_")))
    post (<? (parse-to (parse-constant (parse-eof))))
    (succeed
     (bind (((pre-write/comp pre-emit open-registers closed-registers)
             (funcall (parse-pre categories features valued-features
                                 category-map)
                      pre))
            ((before-write/comp open-registers closed-registers)
             (funcall (parse-before categories features valued-features
                                    category-map
                                    open-registers closed-registers)
                      before))
            ((post-write/comp post-emit closed-registers)
             (funcall (parse-post categories features valued-features
                                  category-map
                                  open-registers closed-registers)
                      post))
            ((after-emit)
             (funcall (parse-after categories features valued-features
                                   category-map
                                   closed-registers)
                      after)))
       `(:sequence ,pre-write/comp ,before-write/comp ,post-write/comp
                   ,pre-emit ,after-emit ,post-emit)))))

(in-package #:mang)

;;; [+obstruent,-voice]1V2/~1<-[front<-2]V2<-[round<-3]/V3_

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

(defun parse-register-feature (features)
  (>>!
    feature (parse-prefix-set features)
    _ (parse-whitespace)
    register (parse-register)
    (succeed `(:register-feature ,register ,feature))))

(defun parse-feature (features)
  (// (parse-binary-feature features)
      (parse-register-feature features)))

(defun parse-feature-set (features)
  (>>!
    _ (parse-constant "[")
    _ (parse-whitespace)
    feature (parse-feature features)
    features (many (>> (parse-whitespace)
                       (parse-constant ",")
                       (parse-whitespace)
                       (parse-feature features))
                   (empty-set)
                   (lambda (val set)
                     (with set val)))
    _ (parse-whitespace)
    _ (parse-constant "]")
    (succeed (with features feature))))

(defun parse-binary-feature-set (features)
  (>>!
    _ (parse-constant "[")
    _ (parse-whitespace)
    feature (parse-binary-feature features)
    features (many (>> (parse-whitespace)
                       (parse-constant ",")
                       (parse-whitespace)
                       (parse-binary-feature features))
                   (empty-set)
                   (lambda (val set)
                     (with set val)))
    _ (parse-whitespace)
    _ (parse-constant "]")
    (succeed (with features feature))))

(defun parse-registerless-filter (type categories features category-map)
  (declare (type symbol type)
           (type set categories features)
           (type map category-map))
  (>>!
    category (parse-category categories)
    features (<? (parse-feature-set features))
    (succeed `(,type ,(@ category-map category)
                     ,(image #'rest
                             (filter (lambda (feature)
                                       (eq (first feature)
                                           :binary-feature))
                                     features))
                     ,(image #'rest
                             (filter (lambda (feature)
                                       (eq (first feature)
                                           :register-feature))
                                     features))
                     nil))))

(defun parse-filter (type categories features category-map)
  (declare (type symbol type)
           (type set categories features)
           (type map category-map))
  (>>!
    category (parse-category categories)
    features (<? (parse-feature-set features))
    register (<? (parse-register))
    (succeed `(,type ,(@ category-map category)
                     ,(image #'rest
                             (filter (lambda (feature)
                                       (eq (first feature)
                                           :binary-feature))
                                     features))
                     ,(image #'rest
                             (filter (lambda (feature)
                                       (eq (first feature)
                                           :register-feature))
                                     features))
                     ,register))))

(defun parse-flat-filter-sequence (type categories features category-map)
  (declare (type set categories features)
           (type map category-map))
  (<$> (lambda (sequence)
         (cons :sequence sequence))
       (some (parse-filter type categories features category-map)
             '() #'cons)))

(defun parse-alternative-filter (type categories features category-map)
  (declare (type set categories features)
           (type map category-map))
  (>>!
    _ (parse-constant "(")
    first (parse-registerless-filter type categories features category-map)
    rest (some (>> (parse-constant "|")
                   (parse-registerless-filter type categories features
                                              category-map))
               '() #'cons)
    _ (parse-constant ")")
    register (<? (parse-register))
    (succeed `(:alternative ,register ,first ,@rest))))

(defun parse-filter-sequence (type categories features category-map)
  (<$> (lambda (sequence)
         (cons :sequence sequence))
       (some (// (parse-filter type categories features category-map)
                 (parse-alternative-filter type categories features
                                           category-map))
             '() #'cons)))

(defun annotate-writes (term &optional (open-registers (empty-set))
                               (closed-registers (empty-set)))
  (bind (((type &rest args)
          term))
    (case type
      (:sequence
       (if args
           (bind (((curr &rest rest)
                   args)
                  ((:values nrterm nwterm nopen nclosed)
                   (annotate-writes curr open-registers closed-registers))
                  ((:values rrterm rwterm ropen rclosed)
                   (annotate-writes `(:sequence ,@rest)
                                    nopen nclosed)))
             (values `(:sequence ,nrterm ,@rrterm)
                     `(:sequence ,nwterm ,@rwterm)
                     ropen rclosed))))
      (:alternative
       (if args
           (bind (((curr &rest rest)
                   args)
                  ((:values nrterm nwterm nopen nclosed)
                   (annotate-writes curr open-registers closed-registers))
                  ((:values rrterm rwterm ropen rclosed)
                   (annotate-writes `(:alternative ,@rest)
                                    open-registers closed-registers)))
             (values `(:alternative ,nrterm ,@rrterm)
                     `(:alternative ,nwterm ,@rwterm)
                     (union nopen ropen)
                     (union nclosed rclosed)))))
      (:matcher
       )
      (t
       (values term nil open-registers closed-registers)))))

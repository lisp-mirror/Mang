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

;;; String -> (String, (writes, reads, registers-written), Bool)
(defun parse-before-match (written-registers categories features category-map)
  (declare (type set written-registers categories features))
  (>>!
    category (parse-category categories)
    register (<? (parse-register))
    features (<? (parse-feature-set features))
    (succeed
     (bind ((binary-features (filter (lambda (feature)
                                       (eq (first feature)
                                           :binary-feature))
                                     features))
            (register-features (filter (lambda (feature)
                                         (eq (first feature)
                                             :register-feature))
                                       features)))
       (todo)))))

(defun parse-before (categories features category-map
                     &optional (written-registers (empty-set)))
  (<? (>>* (parse-before-match written-registers categories features
                               category-map)
           result
           (bind (((writes reads registers-written)
                   result))
             (parse-before categories features category-map
                           (union written-registers registers-written))))))

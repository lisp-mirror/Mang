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

(defun fst<-spec (spec)
  (destructuring-bind (type &rest args)
      spec
    (ecase type
      ;; load
      (:constant  ; t
       (destructuring-bind (phoneme)
           args
         (fst-elementary #'true
                         phoneme
                         :consume? nil)))
      (:load-phoneme  ; .1[+round,front2]
       (destructuring-bind (register &key (features (empty-map))
                                     (features-from-register (empty-map)))
           args
         (fst-elementary #'true
                         (lambda (glyph)
                           (declare (ignore glyph))
                           (load-glyph :phoneme-register register
                                       :constant-features features
                                       :features-from-register
                                       features-from-register))
                         :consume? nil)))
      (:load-category  ; C1[-voice]
       (destructuring-bind (phoneme-register category-register category
                                             &key (features (empty-map))
                                             (features-from-register
                                              (empty-map)))
         args
         (fst-elementary #'true
                         (lambda (glyph)
                           (declare (ignore glyph))
                           (load-glyph :phoneme-register phoneme-register
                                       :category-register category-register
                                       :category category
                                       :constant-features features
                                       :features-from-register
                                       features-from-register))
                         :consume? nil)))
      ;; save
      (:save  ; .1
       (destructuring-bind (phoneme-register)
           args
         (fst-elementary (lambda (glyph)
                           (declare (special *registry*))
                           (setf (gethash phoneme-register *registry*)
                                 glyph)
                           t)
                         '())))
      (:save-by-category  ; C1
       (destructuring-bind (category phoneme-register category-register)
           args
         (fst-elementary (lambda (glyph)
                         (declare (special *registry*))
                         (when (@ category glyph)
                           (setf (gethash phoneme-register *registry*)
                                 glyph
                                 (gethash category-register *registry*)
                                 (position glyph category
                                           :test #'equal?))
                           t))
                         '())))
      (:save-by-features  ; .1[+back]
       (destructuring-bind (features phoneme-register)
           args
         (fst-elementary (lambda (glyph)
                           (declare (special *registry*))
                           (when (has-features? glyph features)
                             (setf (gethash phoneme-register *registry*)
                                   glyph)
                             t))
                         '())))
      (:save-by-category-and-features  ; V1[+back]
       (destructuring-bind (category features phoneme-register
                                     category-register)
           args
         (fst-elementary (lambda (glyph)
                           (declare (special *registry*))
                           (when (and (@ category glyph)
                                      (has-features? glyph features))
                             (setf (gethash phoneme-register *registry*)
                                   glyph
                                   (gethash category-register *registry*)
                                   (position glyph category
                                             :test #'equal?))
                             t))
                         '())))
      ;; filter
      (:filter-by-category  ; C
       (destructuring-bind (category)
           args
         (fst-elementary (lambda (glyph)
                           (@ category glyph))
                         '())))
      (:filter-by-features  ; .[+voice]
       (destructuring-bind (features)
           args
         (fst-elementary (lambda (glyph)
                           (has-features? glyph features))
                         '())))
      (:filter-by-category-and-features  ; C[+labial]
       (destructuring-bind (category features)
           args
         (fst-elementary (lambda (glyph)
                           (and (@ category glyph)
                                (has-features? glyph features)))
                         '())))
      ;; general
      (:sequence
       (if args
           (destructuring-bind (current &rest rest)
               args
             (fst-sequence (apply #'fst<-spec
                                  current)
                           (apply #'fst<-spec
                                  :sequence rest)))
           (empty-fst)))
      (:alternative
       (if args
           (destructuring-bind (current &rest rest)
               args
             (fst-alternate (apply #'fst<-spec
                                   current)
                            (apply #'fst<-spec
                                   :alternative rest)))
           (empty-fst))))))

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

(defun parse-identifier ()
  (some (parse-unicode-property "Alphabetic")))

(defun parse-category (categories)
  (declare (type set categories))
  (// (>>!
        _ (parse-constant "<")
        name (parse-identifier)
        _ (parse-constant ">")
        (succeed name))
      (parse-prefix-set categories)))

(defun parse-number ()
  (some (parse-unicode-property "Number")))

(defun parse-register ()
  (// (parse-number)
      (>>!
        _ (parse-constant "{")
        name (parse-identifier)
        _ (parse-constant "}")
        (succeed name))))

(defun parse-binary-feature (features)
  (>>!
    sign (// (<$ t (parse-constant "+"))
             (parse-constant "-"))
    feature (parse-prefix-set features)
    (succeed `(:signed-feature ,sign ,feature))))

(defun parse-register-feature (features)
  (>>!
    feature (parse-prefix-set features)
    register (parse-register)
    (succeed `(:register-feature ,register ,feature))))

(defun parse-feature (features)
  (// (parse-binary-feature features)
      (parse-register-feature features)))

(defun parse-feature-set (features)
  (>>!
    _ (parse-constant "[")
    feature (parse-feature features)
    features (many (>> (parse-constant ",")
                       (parse-feature features))
                   (empty-set)
                   (lambda (val set)
                     (with set val)))
    (succeed (with features feature))))

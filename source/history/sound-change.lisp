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

(defun save-glyph (&key
                     phoneme-register
                     category category-register
                     features)
  (declare (special *registry*)
           (type symbol phoneme-register category-register)
           (type (or map null)
                 features))
  (assert (or (not category)
              category-register))
  (if category
      (if features
          (lambda (phoneme)
            (declare (special *registry*)
                     (type map phoneme))
            (when (and (in-category? phoneme category)
                       (has-features? phoneme features))
              (setf (gethash category-register *registry*)
                    category
                    (gethash phoneme-register *registry*)
                    phoneme)
              t))
          (lambda (phoneme)
            (declare (special *registry*)
                     (type map phoneme))
            (when (in-category? phoneme category)
              (setf (gethash category-register *registry*)
                    category
                    (gethash phoneme-register *registry*)
                    phoneme)
              t)))
      (if features
          (lambda (phoneme)
            (declare (special *registry*)
                     (type map phoneme))
            (when (has-features? phoneme features)
              (setf (gethash phoneme-register *registry*)
                    phoneme)
              t))
          (lambda (phoneme)
            (declare (special *registry*)
                     (type map phoneme))
            (setf (gethash phoneme-register *registry*)
                  phoneme)
            t))))

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
  (let ((phoneme (or phoneme (gethash phoneme-register *registry*))))
    (map-union
     (map-union
      (if category
          (@ category (position phoneme (gethash category-register *registry*)))
          phoneme)
      (image (lambda (feature register)
               (values feature (@ (gethash register *registry*)
                                  feature)))
             features-from-register))
     constant-features)))

(defun fst<-spec (type &rest args)
  (ecase type
    ;; load
    (:constant  ; t
     (destructuring-bind (phoneme)
         args
       (fst-elementary #'true
                       (constantly `(,phoneme))
                       :consume nil)))
    (:load-phoneme  ; .1[+round,front2]
     (destructuring-bind (register &key (features (empty-map))
                                   (features-from-register (empty-map)))
         args
       (fst-elementary #'true
                       (load-glyph :phoneme-register register
                                   :constant-features features
                                   :features-from-register
                                   features-from-register)
                       :consume nil)))
    (:load-category  ; C1[-voice]
     (destructuring-bind (phoneme-register category-register category
                                           &key (features (empty-map))
                                           (features-from-register (empty-map)))
         args
       (fst-elementary #'true
                       (load-glyph :phoneme-register phoneme-register
                                   :category-register category-register
                                   :category category
                                   :constant-features features
                                   :features-from-register
                                   features-from-register)
                       :consume nil)))
    ;; save
    (:save  ; .1
     (destructuring-bind (phoneme-register category-register)
         args
       (fst-elementary #'true
                       (save-glyph :phoneme-register phoneme-register))))
    (:save-by-category  ; C1
     (destructuring-bind (category phoneme-register category-register)
         args
       (fst-elementary (lambda (phoneme)
                         (in-category? phoneme category))
                       (save-glyph :phoneme-register phoneme-register
                                   :category-register category-register
                                   :category category))))
    (:save-by-features  ; .1[+back]
     (destructuring-bind (features phoneme-register)
         args
       (fst-elementary (lambda (phoneme)
                         (has-features? phoneme features))
                       (save-glyph :phoneme-register phoneme-register))))
    (:save-by-category-and-features  ; V1[+back]
     (destructuring-bind (category features phoneme-register category-register)
         args
       (fst-elementary (lambda (phoneme)
                         (and (in-category? phoneme category)
                              (has-features? phoneme features)))
                       (save-glyph :phoneme-register phoneme-register
                                   :category-register category-register
                                   :category category))))
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
         (empty-fst)))
    ))

(defmethod apply-sound-change ((word word)
                               (sound-change fst))
  (let ((*registry* (make-hash-table :test 'eq)))
    (declare (special *registry*))
    (word (mapcar #'funcall
                  (run-fst sound-change (form<- word))
                  :origin word
                  :transformations (transformations<- word)))))

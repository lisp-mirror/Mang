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
    (:constant
     (destructuring-bind (phoneme)
         args
       (fst-elementary #'true
                       (constantly `(,phoneme))
                       :consume nil)))
    (:load-phoneme
     (destructuring-bind (register)
         args
       (fst-elementary #'true
                       (load-glyph :phoneme-register register)
                       :consume nil)))
    (:load-category
     (destructuring-bind (phoneme-register category-register category)
         args
       (fst-elementary #'true
                       (load-glyph :phoneme-register phoneme-register
                                   :category-register category-register
                                   :category category)
                       :consume nil)))
    ;; TODO
    ;; save
    ;; TODO
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
    ))

#+nil
(defun save-glyph (condition)
  (declare (special *registry*))
  (let ((register (gensym "register")))
    (values (lambda (glyph)
              (when (funcall condition glyph)
                (setf (gethash register *registry*)
                      glyph)
                t))
            register)))
#+nil
(defun load-register (base-register source-cat target-cat register-features
                      constant-features)
  (declare (special *registry*))
  (map-union (elt target-cat (position (gethash base-register *registry*)
                                       source-cat))
             (map-union (image (lambda (k v)
                                 (values k (gethash v *registry*)))
                               register-features)
                        constant-features)))
#+nil
(defun fst<-lhs-spec (spec)
  (if (consp spec)
      (destructuring-bind (type &rest args)
          spec
        (case type
          ))
      (fst-elementary (lambda (glyph)
                        (equal? glyph spec)))))
#+nil
(defun sound-change (to-replace replacement pre post)
  (declare (special *registry*))
  (let ((start (gensym "start-sound-change"))
        (finish (gensym "finish-sound-change"))
        (inner1 (gensym "sound-change-inner-in"))
        (inner2 (gensym "sound-change-inner-out")))
    (modify-fst
     (add-epsilon-transitions
      (fst-preferred
       (build-sca-fst to-replace replacement pre post)
       (multiple-value-bind (condition register)
           (save-glyph (constantly t))
         (fst-elementary condition (load-register register)))
       :in-state inner1
       :out-state inner2)
      start finish
      start inner1
      inner2 inner1
      inner2 finish))))

(defmethod apply-sound-change ((word word)
                               (sound-change fst))
  (let ((*registry* (make-hash-table :test 'eq)))
    (declare (special *registry*))
    (word (mapcar #'funcall
                  (run-fst sound-change (form<- word))
                  :origin word
                  :transformations (transformations<- word)))))

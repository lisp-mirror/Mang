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

(defparameter +whitespace+
  `(:greedy-repetition 0 nil :whitespace-char-class))

(defparameter +identifier-parse-tree+
  `(:greedy-repetition
    0 nil
    (:alternation
     (:char-class (:range #\A #\Z))
     (:char-class (:range #\a #\z)))))

(defparameter +category-parse-tree+
  `(:alternation
    (:alternation (:char-class (:range #\A #\Z))
                  (:char-class (:range #\a #\z)))
    (:sequence
     "<"
     ,+identifier-parse-tree+
     ">")))

(defparameter +number-parse-tree+
  `(:greedy-repetition 1 nil (:char-class :digit-class)))

(defparameter +feature-reference-parse-tree+
  `(:sequence ,+identifier-parse-tree+
              ,+number-parse-tree+))

(defparameter +binary-feature-parse-tree+
  `(:sequence (:alternation "+" "-")
              ,+identifier-parse-tree+))

(defparameter +feature-parse-tree+
  `(:alternation ,+feature-reference-parse-tree+ ,+binary-feature-parse-tree+))

(defparameter +features-list-parse-tree+
  `(:sequence
    "["
    ,+whitespace+
    ,+feature-parse-tree+
    (:greedy-repetition 0 nil
                        (:sequence
                         ,+whitespace+
                         ","
                         ,+whitespace+
                         ,+feature-parse-tree+))
    ,+whitespace+
    "]"))

(defparameter +sound-change-token-parse-tree+
  `(:alternation
    ,+category-parse-tree+
    ,+number-parse-tree+
    ,+features-list-parse-tree+
    "." "(" "|" ")" "*" "?" "->" "→" "/" "_" "#"))

(defun tokenize-by (parse-tree string)
  (let ((string (regex-replace-all +whitespace+ string "")))
    (if (string= string "")
        '()
        (bind (((:values start end)
                (scan parse-tree string)))
          (if (= start end)
              (list `(:tokenize-error ,string))
              (bind ((token (subseq string start end))
                     (rest (subseq string end)))
                (if (= start 0)
                    (cons token (tokenize-by parse-tree rest))
                    (list* `(:tokenize-error ,(subseq string 0 start))
                           token (tokenize-by parse-tree rest)))))))))

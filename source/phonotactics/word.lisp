(in-package #:mang)

(defun parse-expression-end ()
  (// (parse-newline)
      (parse-eof)))

(defun parse-category (categories)
  (declare (type map categories))
  (// (>>!
        _ (>> (parse-whitespace)
              (parse-constant "<")
              (parse-whitespace))
        name (parse-identifier *mang-reserved-symbols*)
        _ (>> (parse-whitespace)
              (parse-constant ">"))
        ([av]if (@ categories name)
            (succeed it)
          `(:unknown-category ,name)))
      (>>* (parse-unicode-property "Letter")
           name
           ([av]if (@ categories name)
               (succeed it)
             (fail `(:unknown-category ,name))))))

(defun parse-syllable-generator (categories)
  (declare (type map categories))
  (labels ((_alternatives ()
             (>>!
               _ (parse-whitespace)
               front (parse-syllable-generator categories)
               back (many (>> (parse-whitespace)
                              (parse-constant "|")
                              (parse-whitespace)
                              (<? (parse-syllable-generator categories)))
                          (empty-set)
                          (lambda (front back)
                            (with back front)))
               (succeed (with back front)))))
    (>>!
      _ (parse-whitespace)
      front (// (<$> (parse-category categories)
                     (lambda (category)
                       (convert 'set
                                category)))
                (>>!
                  _ (parse-constant "(")
                  alternatives (_alternatives)
                  _ (>> (parse-whitespace)
                        (parse-constant ")"))
                  (succeed alternatives)))
      back (<? (parse-syllable-generator categories))
      (succeed `(,front ,@back)))))

(defun parse-syllable-definition (categories)
  (declare (type map categories))
  (>>!
    _ (parse-whitespace)
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    definition (parse-syllable-generator categories)
    _ (parse-expression-end)
    (succeed `(,name ,definition))))

(defun parse-syllable-definitions (categories)
  (declare (type map categories))
  (many (parse-syllable-definition categories)
        (empty-map)
        (lambda (definition definitions)
          (bind (((name definition)
                  definition))
            (with definitions name definition)))))

(defun parse-syllables-spec (syllables)
  (declare (type map syllables))
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "(")
          (parse-whitespace))
    definition (>>!
                 name (parse-identifier *mang-reserved-symbols*)
                 ([av]if (@ syllables name)
                     (succeed it)
                   (fail `(:unknown-syllable-type ,name))))
    _ (>> (parse-whitespace)
          (parse-constant ")")
          (parse-whitespace))
    count (<? (<$> (parse-number)
                   #'parse-integer)
              1)
    (succeed (apply #'append
                    (repeat count definition)))))

(defun parse-syllables-word-spec (syllables)
  (some (parse-syllables-spec syllables)
        '() #'append))

(defmethod word-forms<-spec ((spec null))
  (set '()))

(defmethod word-forms<-spec ((spec cons))
  (bind (((min/repeat/syllable &rest spec)
          spec))
    (if (integerp min/repeat/syllable)
        (bind (((max/syllable &rest spec)
                spec))
          (if (integerp max/syllable)
              (bind (((syllable &rest spec)
                      spec))
                (expand (lambda (tail)
                          (image (lambda (intro)
                                   (append intro tail))
                                 (var-repeat min/repeat/syllable
                                             max/syllable syllable)))
                        (word-forms<-spec spec)))
              (image (lambda (tail)
                       (append (repeat min/repeat/syllable max/syllable)
                               tail))
                     (word-forms<-spec spec))))
        (image (lambda (tail)
                 (cons min/repeat/syllable tail))
               (word-forms<-spec spec)))))

(defmethod word-forms<-spec ((spec set))
  (expand #'word-forms<-spec
          spec))

(defun words<-spec (spec classes)
  (image (lambda (word)
           (intersperse "" word))
         (tree-map classes (word-forms<-spec spec))))

(defmodel word-system ()
  ((%spec :type (or symbol cons set)
          :accessor spec<-
          :initarg :spec)
   (%glyph-system :type glyph-system
                  :accessor glyph-system<-
                  :initarg :glyph-system)
   (%dfsm :type dfsm
          :reader dfsm<-
          :initform
          (c? (dfsm<- (words<-spec (spec<- self)
                                   (classes<- (glyph-system<- self))))))))

(defun word-system (spec glyph-system)
  (make-instance 'word-system
                 :spec (c_in spec)
                 :glyph-system (c_in glyph-system)))

(defmethod generate ((generator word-system)
                     (markov map)
                     &optional negative)
  (declare (type (or map null)
                 negative))
  (generate (dfsm<- generator)
            (with markov
                  (constantly t)
                  (uniform-distribution (set "")))
            (when negative
              (with negative
                    (constantly t)
                    (uniform-distribution (set ""))))))

(defmethod run-dfsm ((dfsm word-system)
                     word)
  (run-dfsm (dfsm<- dfsm)
            word))

(defmethod string<-word ((word cons)
                         &optional syllables?)
  (format nil "~{~A~}"
          (if syllables?
              (mapcar (lambda (glyph)
                        (if (string= glyph "")
                            "."
                            glyph))
                      word)
              word)))

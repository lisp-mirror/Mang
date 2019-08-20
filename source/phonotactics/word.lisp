(in-package #:mang)

(defun parse-expression-end ()
  (//!
    _ (// (parse-newline)
          (parse-eof))
    _ (>> (parse-unicode-property "Whitespace")
          (parse-expression-end))
    (fail `(:expression-not-over))))

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
    (min max)
    (<? (>>!
          min (<$> (parse-number)
                   #'parse-integer)
          max (<? (>> (parse-whitespace)
                      (parse-constant "-")
                      (parse-whitespace)
                      (<$> (parse-number)
                           #'parse-integer))
                  min)
          (if (<= min max)
              (succeed `(,min ,max))
              (fail `(:syllable-count-malfored ,(first definition)
                                               ,min ,max))))
        `(1 1))
    (succeed (convert 'set
                      (loop :for count :from min :to max
                         :collect (apply #'append
                                         (repeat count definition)))))))

(defun parse-syllables-word-spec (syllables)
  (some (parse-syllables-spec syllables)
        (set '())
        (lambda (syls words)
          (cross-product #'append
                         syls words))))

(defmethod string<-word ((word cons)
                         (glyphs map))
  (format nil "~{~A~}"
          (image (lambda (phoneme)
                   (arb (origin phoneme glyphs)))
                 word)))

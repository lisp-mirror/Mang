(in-package #:mang)

(defun parse-expression-end ()
  (//!
    _ (// (parse-newline)
          (parse-eof))
    _ (>> (parse-unicode-property "Whitespace")
          (parse-expression-end))
    (fail `(:expression-not-over))))

(defun parse-category-definition (glyphs)
  (>>!
    _ (parse-whitespace)
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    glyph (>>!
            name (parse-identifier *mang-reserved-symbols*)
            ([av]if (@ glyphs name)
                (succeed it)
              (fail `(:unknown-glyph ,name))))
    glyphs (many (>>!
                   _ (>> (parse-whitespace)
                         (parse-constant ",")
                         (parse-whitespace))
                   name (parse-identifier *mang-reserved-symbols*)
                   ([av]if (@ glyphs name)
                       (succeed it)
                     (fail `(:unknown-glyph ,name))))
                 '() #'cons)
    _ (parse-expression-end)
    (succeed `(,name (,glyph ,@glyphs)))))

(defun parse-category-definitions (glyphs)
  (>> (parse-whitespace)
      (parse-constant "#categories:")
      (parse-newline)
      (parse-whitespace)
      (many (parse-category-definition glyphs)
            (empty-map (empty-map))
            (lambda (category categories)
              (bind (((name contents)
                      category))
                (with categories name contents))))))

(defun parse-category (categories)
  (declare (type map categories))
  (// (>>!
        _ (>> (parse-whitespace-no-newline)
              (parse-constant "<")
              (parse-whitespace-no-newline))
        name (parse-identifier *mang-reserved-symbols*)
        _ (>> (parse-whitespace-no-newline)
              (parse-constant ">"))
        ([av]if (@ categories name)
            (succeed it)
          (fail `(:unknown-category ,name))))
      (>>!
        _ (parse-whitespace-no-newline)
        name (parse-unicode-property "Letter")
        ([av]if (@ categories name)
            (succeed it)
          (fail `(:unknown-category ,name))))))

(defun parse-glyph-for-generator (glyphs)
  (>>!
    _ (parse-whitespace-no-newline)
    glyph (// (>>!
                _ (>> (parse-constant "<")
                      (parse-whitespace-no-newline))
                glyph (parse-identifier *mang-reserved-symbols*)
                _ (>> (parse-whitespace-no-newline)
                      (parse-constant ">"))
                (succeed glyph))
              (parse-anything))
    ([av]if (@ glyphs glyph)
        (succeed it)
      (fail `(:unknown-glyph ,glyph)))))

(defun parse-syllable-generator (glyphs categories)
  (declare (type map categories))
  (labels ((_alternatives ()
             (>>!
               _ (parse-whitespace-no-newline)
               front (parse-syllable-generator glyphs categories)
               back (many (>> (parse-whitespace-no-newline)
                              (parse-constant "|")
                              (parse-whitespace-no-newline)
                              (parse-syllable-generator glyphs categories))
                          (empty-set)
                          (lambda (front back)
                            (with back front)))
               (succeed (with back front)))))
    (>>!
      _ (parse-whitespace-no-newline)
      front (// (<$> (parse-category categories)
                     (lambda (category)
                       (convert 'set
                                category)))
                (parse-glyph-for-generator glyphs)
                (>>!
                  _ (parse-constant "[")
                  alternatives (_alternatives)
                  _ (>> (parse-whitespace-no-newline)
                        (parse-constant "]"))
                  (succeed alternatives))
                (>>!
                  _ (parse-constant "(")
                  alternatives (_alternatives)
                  _ (>> (parse-whitespace-no-newline)
                        (parse-constant ")"))
                  (succeed (with alternatives '()))))
      back (<? (parse-syllable-generator glyphs categories))
      (succeed `(,front ,@back)))))

(defun parse-syllable-definition (glyphs categories)
  (declare (type map categories))
  (>>!
    _ (parse-whitespace)
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    definition (parse-syllable-generator glyphs categories)
    _ (parse-expression-end)
    (succeed `(,name ,definition))))

(defun parse-syllable-definitions (glyphs categories)
  (declare (type map categories))
  (>> (parse-whitespace)
      (many (parse-syllable-definition glyphs categories)
            (empty-map)
            (lambda (definition definitions)
              (bind (((name definition)
                      definition))
                (with definitions name definition))))))

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

(defun parse-syllables-generator (glyphs categories)
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "#syllables-generator:")
          (parse-whitespace)
          (parse-constant "#syllable-specs:")
          (parse-expression-end)
          (parse-whitespace))
    syllables (parse-syllable-definitions glyphs categories)
    _ (>> (parse-whitespace)
          (parse-constant "#word-specs:")
          (parse-expression-end)
          (parse-whitespace))
    word-spec (parse-syllables-word-spec syllables)
    word-specs (many (>> (parse-expression-end)
                         (parse-whitespace)
                         (parse-syllables-word-spec syllables))
                     (empty-set)
                     (lambda (word-spec word-specs)
                       (with word-specs word-spec)))
    _ (parse-expression-end)
    (succeed (dfsm<- (with word-specs word-spec)))))

(defmethod parse-cluster-definition (glyphs categories)
  (labels ((_alternatives ()
             (>>!
               _ (parse-whitespace)
               front (parse-cluster-definition glyphs categories)
               back (many (>> (parse-whitespace)
                              (parse-constant "|")
                              (parse-whitespace)
                              (_alternatives))
                          (empty-set)
                          (lambda (alternative alternatives)
                            (with alternatives alternative)))
               (succeed (with back front)))))
    (// (>> (parse-whitespace)
            (parse-constant "()"))
        (>>!
          _ (parse-whitespace)
          front (// (<$> (parse-category categories)
                         (lambda (category)
                           (convert 'set
                                    category)))
                    (<$> (parse-glyph-for-generator glyphs)
                         (lambda (glyph)
                           (set glyph)))
                    (>>!
                      _ (parse-constant "(")
                      alternatives (_alternatives)
                      _ (>> (parse-whitespace)
                            (parse-constant ")"))
                      (succeed (with alternatives '())))
                    (>>!
                      _ (parse-constant "[")
                      alternatives (_alternatives)
                      _ (>> (parse-whitespace)
                            (parse-constant "]"))
                      (succeed alternatives)))
          back (<? (parse-cluster-definition glyphs categories))
          (succeed `(,front ,@back))))))

(defun parse-cluster-definitions (glyphs categories)
  (>>!
    _ (parse-whitespace)
    front (parse-cluster-definition glyphs categories)
    _ (// (parse-expression-end)
          (>> (parse-whitespace)
              (parse-constant "|")
              (parse-whitespace)))
    back (<? (parse-cluster-definitions glyphs categories)
             (empty-set))
    (succeed (with back front))))

(defun parse-clusters-generator (glyphs categories)
  (>>!
    _ (>> (parse-whitespace)
          (parse-constant "#cluster-generator:")
          (parse-expression-end)
          (parse-whitespace))
    (min max)
    (// (>>!
          _ (>> (parse-constant "#min:")
                (parse-whitespace))
          min (>>!
                min (parse-number)
                _ (parse-expression-end)
                (succeed (parse-integer min)))
          max (>>!
                _ (>> (parse-whitespace)
                      (parse-constant "#max:")
                      (parse-whitespace))
                max (parse-number)
                _ (parse-expression-end)
                ([d]if (>= (parse-integer max)
                           min)
                    (succeed (parse-integer max))
                  (fail `(:malformed-word-length ,min ,max))))
          (succeed `(,min ,max)))
        (>> (parse-constant "#count:")
            (parse-whitespace)
            (>>!
              count (parse-number)
              _ (parse-expression-end)
              (succeed (bind ((count (parse-integer count)))
                         `(,count ,count))))))
    _ (>> (parse-whitespace)
          (parse-constant "#begin:")
          (parse-newline))
    begin (parse-cluster-definitions glyphs categories)
    _ (>> (parse-whitespace)
          (parse-constant "#mid:")
          (parse-newline))
    mid (parse-cluster-definitions glyphs categories)
    _ (>> (parse-whitespace)
          (parse-constant "#end:")
          (parse-newline))
    end (parse-cluster-definitions glyphs categories)
    _ (>> (parse-whitespace)
          (parse-constant "#nuclei:")
          (parse-newline))
    nuclei (parse-cluster-definitions glyphs categories)
    _ (parse-expression-end)
    (succeed (dfsm<- (image (lambda (count)
                              (list begin (intersperse mid (repeat count
                                                                   nuclei))
                                    end))
                            (convert 'set
                                     (loop :for count :from min :to max
                                        :collect count)))))))

(defun load-generator (file glyphs)
  (with-open-file (stream file)
    (parser-call (>>!
                   categories (parse-category-definitions glyphs)
                   generator (// (parse-syllables-generator glyphs categories)
                                 (parse-clusters-generator glyphs categories))
                   _ (>> (parse-whitespace)
                         (parse-eof))
                   (succeed generator))
                 stream)))

(defmethod string<-word ((word cons)
                         (glyphs map))
  (format nil "~{~A~}"
          (image (lambda (phoneme)
                   (arb (origin phoneme glyphs)))
                 word)))

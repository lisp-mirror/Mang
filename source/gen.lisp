(in-package #:mang)

(defun parse-syllable-generator (glyphs categories)
  (//!
    e1 (parse-constant "()")
    e2 (>>!
         front (// (<$> (parse-category categories)
                        (lambda (category)
                          (convert 'set
                                   category)))
                   (parse-glyph glyphs)
                   (parse-wrapped "["
                                  (parse-separated (parse-syllable-generator
                                                    glyphs categories)
                                                   "|" (empty-set)
                                                   (lambda (gen gens)
                                                     (with gens gen)))
                                  "]")
                   (<$> (parse-wrapped "("
                                       (parse-separated
                                        (parse-syllable-generator glyphs
                                                                  categories)
                                        "|" (empty-set)
                                        (lambda (gen gens)
                                          (with gens gen)))
                                       ")")
                        (lambda (options)
                          (with options '()))))
         _ (parse-whitespace-no-newline)
         back (<? (parse-syllable-generator glyphs categories))
         (succeed `(,front ,@back)))
    (fail `(,e1 ,e2))))

(defun parse-syllable-definition (glyphs categories)
  (>>!
    name (parse-identifier *mang-reserved-symbols*)
    definition (>> (parse-whitespace)
                   (parse-constant ":=")
                   (parse-whitespace)
                   (parse-syllable-generator glyphs categories))
    (succeed `(,name ,definition))))

(defun parse-syllable-block (name glyphs categories)
  (parse-subsection name
                    (parse-lines (parse-syllable-definition glyphs categories)
                                 (empty-map)
                                 (lambda (syllable syllables)
                                   (bind (((name gen)
                                           syllable))
                                     (with syllables name gen))))))

(defun parse-wordgen-spec (syllables)
  (>>!
    (_ gen)
    (parse-from-map syllables)
    _ (parse-whitespace-no-newline)
    (min max)
    (<? (// (>>!
              min (<$> (parse-number)
                       #'parse-integer)
              _ (>> (parse-whitespace-no-newline)
                    (parse-constant "-")
                    (parse-whitespace-no-newline))
              max (<$> (parse-number)
                       #'parse-integer)
              (if (>= max min)
                  (succeed `(,min ,max))
                  (fail `(:malformed-range ,min ,max))))
            (<$> (parse-number)
                 (lambda (count)
                   (bind ((count (parse-integer count)))
                     `(,count ,count)))))
        `(1 1))
    _ (parse-whitespace-no-newline)
    back (parse-wordgen-spec syllables)
    (succeed (image (lambda (count)
                      `(,@(repeat count gen)
                          ,@back))
                    (convert 'set
                             (loop :for n :from min :to max
                                :collect n))))))

(defun parse-wordgen-specs-block (syllables)
  (parse-subsection "word-generators"
                    (parse-lines (parse-wordgen-spec syllables)
                                 (empty-set)
                                 #'union)))

(defun parse-wordgen-block (glyphs categories)
  (>>!
    syllables (parse-syllable-block "syllables" glyphs categories)
    gen (parse-wordgen-specs-block syllables)
    (succeed (dfsm<- gen))))

(defun parse-clustergen-block (glyphs categories)
  (>>!
    nuclei (parse-syllable-block "nuclei" glyphs categories)
    begin (parse-syllable-block "begin" glyphs categories)
    middle (parse-syllable-block "middle" glyphs categories)
    end (parse-syllable-block "end" glyphs categories)
    (min max)
    (>> (parse-whitespace)
        (parse-constant "#")
        (parse-whitespace-no-newline)
        (// (>>!
              _ (>> (parse-constant "min")
                    (parse-whitespace-no-newline)
                    (parse-constant ":")
                    (parse-whitespace-no-newline))
              min (<$> (parse-number)
                       #'parse-integer)
              _ (>> (parse-expression-end)
                    (parse-whitespace)
                    (parse-constant "max")
                    (parse-whitespace-no-newline)
                    (parse-constant ":")
                    (parse-whitespace-no-newline))
              max (<$> (parse-number)
                       #'parse-integer)
              (if (>= max min)
                  (succeed `(,min ,max))
                  (fail `(:malformed-range ,min ,max))))
            (>> (parse-constant "count")
                (parse-whitespace-no-newline)
                (parse-constant ":")
                (parse-whitespace-no-newline)
                (<$> (parse-number)
                     (lambda (count)
                       (bind ((count (parse-integer count)))
                         `(,count ,count)))))))
    (succeed (dfsm<- (image (lambda (count)
                              (list begin (intersperse middle
                                                       (repeat count nuclei))
                                    end))
                            (convert 'set
                                     (loop :for n :from min :to max
                                        :collect n)))))))
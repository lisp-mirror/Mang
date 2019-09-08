(in-package #:mang)

(defun parse-syllable-generator (glyphs categories)
  (//_
      (parse-constant "()")
    (>>!
      front (// (<$> (parse-category categories)
                     (lambda (category)
                       (convert 'set
                                (second category))))
                (<$> (parse-glyph glyphs)
                     #'second)
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
      (succeed `(,front ,@back)))))

(defun parse-syllable-definition (glyphs categories)
  (>>!
    name (parse-identifier *mang-reserved-symbols*)
    definition (>> (parse-whitespace)
                   (parse-constant ":=")
                   (parse-whitespace)
                   (parse-syllable-generator glyphs categories))
    (succeed `(,name ,definition))))

(defun parse-syllable-section (name glyphs categories)
  (parse-section name
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
              min (parse-number)
              _ (>> (parse-whitespace-no-newline)
                    (parse-constant "-")
                    (parse-whitespace-no-newline))
              max (parse-number)
              (if (>= max min)
                  (succeed `(,min ,max))
                  (fail `(:malformed-range ,min ,max))))
            (<$> (parse-number)
                 (lambda (count)
                   `(,count ,count))))
        `(1 1))
    _ (parse-whitespace-no-newline)
    back (<? (parse-wordgen-spec syllables))
    (succeed (image (lambda (count)
                      `(,@(repeat count gen)
                          ,@back))
                    (convert 'set
                             (loop :for n :from min :to max
                                :collect n))))))

(defun parse-wordgen-specs-section (syllables)
  (parse-section "word-generators"
                 (parse-lines (parse-wordgen-spec syllables)
                              (empty-set)
                              #'union)))

(defun parse-wordgen-section (glyphs categories)
  (>>!
    syllables (parse-syllable-section "syllables" glyphs categories)
    gen (parse-wordgen-specs-section syllables)
    (succeed (dfsm<- `(,(map (:begin t))
                        ,gen
                        ,(map (:end t)))))))

(defun parse-clustergen-section (glyphs categories)
  (>>!
    nuclei (parse-syllable-section "nuclei" glyphs categories)
    begin (parse-syllable-section "begin" glyphs categories)
    middle (parse-syllable-section "middle" glyphs categories)
    end (parse-syllable-section "end" glyphs categories)
    (min max)
    (>> (parse-whitespace)
        (parse-constant "#")
        (parse-whitespace-no-newline)
        (// (>>!
              _ (>> (parse-constant "min")
                    (parse-whitespace-no-newline)
                    (parse-constant ":")
                    (parse-whitespace-no-newline))
              min (parse-number)
              _ (>> (parse-expression-end)
                    (parse-whitespace)
                    (parse-constant "max")
                    (parse-whitespace-no-newline)
                    (parse-constant ":")
                    (parse-whitespace-no-newline))
              max (parse-number)
              (if (>= max min)
                  (succeed `(,min ,max))
                  (fail `(:malformed-range ,min ,max))))
            (>> (parse-constant "count")
                (parse-whitespace-no-newline)
                (parse-constant ":")
                (parse-whitespace-no-newline)
                (<$> (parse-number)
                     (lambda (count)
                       `(,count ,count))))))
    (succeed (dfsm<- `(,(map (:begin t))
                       ,(image (lambda (count)
                                 (list begin (intersperse middle
                                                          (repeat count nuclei))
                                       end))
                               (convert 'set
                                        (loop :for n :from min :to max
                                           :collect n)))
                        ,(map (:end t)))))))

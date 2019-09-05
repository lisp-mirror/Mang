(in-package #:mang)

(defun parse-word (glyphs)
  (declare (type map glyphs))
  (some (<$> (parse-glyph glyphs)
             #'second)
        '() #'cons))

(defun parse-gloss (glyphs dfsm store markov-spec)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm))
  (>>!
    gloss (parse-identifier  *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    word (// (parse-word glyphs)
             (parse-constant "#"))
    _ (parse-whitespace)
    (categories negative-categories)
    (<? (>>!
          categories (parse-wrapped "{" (parse-separated (parse-from-map store)
                                                         "," (empty-set)
                                                         (lambda (cat cats)
                                                           (with cats
                                                                 (first cat))))
                                    "}")
          negative-categories
          (<? (>> (parse-whitespace)
                  (parse-wrapped "{" (parse-separated (parse-from-map store)
                                                      "," (empty-set)
                                                      (lambda (cat cats)
                                                        (with cats
                                                              (first cat))))
                                 "}")))
          (succeed `(,categories ,negative-categories)))
        `(nil nil))
    (if word
        (if negative-categories
            (fail `(:save-with-negative-categories ,gloss ,word
                                                   ,negative-categories))
            (succeed `(,(map (gloss word))
                        ,(learn store markov-spec word
                                (or categories (empty-set))))))
        (if categories
            (bind ((word (generate-word dfsm store categories
                                        (or negative-categories (empty-set)))))
              (succeed `(,(map (gloss word))
                          ,(learn store markov-spec word categories))))
            (fail `(:generate-without-categories ,gloss))))))

(defun parse-glosses (glyphs dfsm store markov-spec)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm))
  (>>!
    (front store)
    (parse-gloss glyphs dfsm store markov-spec)
    back (<? (>> (parse-expression-end)
                 (parse-whitespace)
                 (parse-glosses glyphs dfsm store markov-spec)))
    (succeed (map-union back front))))

(defun parse-dictionary (glyphs dfsm store markov-spec)
  (declare (type map glyphs store markov-spec)
           (type dfsm dfsm))
  (parse-section "dictionary" (parse-glosses glyphs dfsm store markov-spec)))

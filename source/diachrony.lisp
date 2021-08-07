(in-package #:mang)

(defun parse-semantic-shift (source-language target-language)
  (>>!
    source-glosses (parse-separated (parse-gloss)
                                    "+")
    _ (>> (parse-whitespace-no-newline)
          (parse-constant "=>")
          (parse-whitespace-no-newline))
    target-gloss (parse-gloss)
    word-categories
    (<? (parse-wrapped "{"
                       (parse-separated (parse-from-map markov-spec)
                                        "," (empty-set)
                                        (lambda (cat cats)
                                          (with cats (first cat))))
                       "}")
        (empty-set))
    (succeed `(,source-glosses ,target-gloss ,word-categories))))

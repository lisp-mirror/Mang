(in-package #:mang)

(defun parse-part-of-speech ()
  (>>!
    pos (parse-wrapped "[" (parse-identifier *mang-reserved-symbols*)
                       "]")
    register (<? (parse-number)
                 nil)
    (succeed (if register
                 `(:register ,register (:part-of-speech ,pos))
                 `(:part-of-speech ,pos)))))

(defun parse-semantic-shift-target ()
  (some (// (<$> (parse-wrapped "[" (parse-number)
                                "]")
                 (lambda (register)
                   `(:register ,register)))
            (parse-identifier (set "-" "[" "]")))
        '() #'cons))

(defun parse-semantic-shift (target-language)
  (>>!
    source-glosses (parse-separated (// (<$> (parse-gloss)
                                             (lambda (gloss)
                                               `(:gloss ,gloss)))
                                        (parse-part-of-speech))
                                    "+")
    _ (>> (parse-whitespace-no-newline)
          (parse-constant "=>")
          (parse-whitespace-no-newline))
    target (parse-semantic-shift-target)
    _ (parse-whitespace-no-newline)
    word-categories
    (<? (parse-w/s "{" (parse-from-set (domain (markov-spec<- target-language)))
                   "," "}" (empty-set)
                   (lambda (cat cats)
                     (with cats cat)))
        (empty-set))
    (succeed `(,source-glosses ,target ,word-categories))))

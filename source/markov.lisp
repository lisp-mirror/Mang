(in-package #:mang)

;;;; A markov spec returns a function taking one argument (which is an initial
;;;; part of a word) which returns three values. If the word part shouldn't be
;;;; learned by the markov chain specified by the spec, the three values should
;;;; be NIL. If the markov spec can learn something from the given word part,
;;;; the second return value is the outro to be learned, the third value T and
;;;; the first value a predicate. This predicate should succeed on intros which
;;;; should be able to receive the outro and fail otherwise. The predicate
;;;; should also be memoized.
(bind ((memoized (empty-map)))
  (defun parse-simple-markov-spec (glyphs categories)
    (declare (type map categories))
    (>>!
      intro (parse-number)
      outro (<? (>> (parse-whitespace-no-newline)
                    (parse-constant ":")
                    (parse-whitespace-no-newline)
                    (parse-number))
                1)
      filter (<? (<$> (>> (parse-whitespace-no-newline)
                          (parse-constant "~")
                          (parse-whitespace-no-newline)
                          (parse-category categories))
                      (lambda (cat)
                        (convert 'set
                                 (second cat))))
                 (range glyphs))
      (bind ((match-length (+ intro outro)))
        (assert (> match-length 0))
        (succeed (set
                  (lambda (word)
                    (bind ((word (filter filter word)))
                      (if (length>= word match-length)
                          (bind ((in+out (subseq (reverse word)
                                                 0 match-length))
                                 (intro (reverse (subseq in+out outro)))
                                 (outro (reverse (subseq in+out 0 outro))))
                            (values
                             (bind ((result (or (@ memoized `(:intro ,intro
                                                                     ,filter))
                                                (lambda (word)
                                                  (postfix? intro
                                                            (filter filter
                                                                    word))))))
                               (setf memoized
                                     (with memoized `(:intro ,intro)
                                           result))
                               result)
                             outro t))
                          (values nil nil nil))))))))))

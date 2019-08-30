(in-package #:mang)

;;;; A markov spec returns a function taking one argument (which is an initial
;;;; part of a word) which returns two values. If the word part shouldn't be
;;;; learned by the markov chain specified by the spec, the two values should be
;;;; NIL. If the markov spec can learn something from the given word part, the
;;;; first return value is the outro to be learned and the second value a
;;;; predicate. This predicate should succeed on intros which should be able to
;;;; receive the outro and fail otherwise. The predicate should also be
;;;; memoized.
(with-memoization
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
                    (if (length>= word match-length)
                        (bind ((rev-word (reverse word))
                               (rev-init (filter filter
                                                 (subseq rev-word outro))))
                          (if (length>= rev-init intro)
                              (bind ((intro (reverse (subseq rev-init 0 intro)))
                                     (outro (reverse (subseq rev-word 0
                                                             outro))))
                                (values outro
                                        (memoized (intro filter)
                                                  (lambda (word)
                                                    (postfix? intro
                                                              (filter filter
                                                                      word))))))
                              (values nil nil)))
                        (values nil nil)))))))))

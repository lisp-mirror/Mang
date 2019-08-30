(in-package #:mang)

;;;; everything := 3:1~C 1000> 2:1~C 1000> 1:1~C 1000> 1:1 500> 0:1,
;;;;               2:1~V 1000> 1:1~V 1000> 1:1 500> 0:1

;;;; A markov spec returns a function taking one argument (which is an initial
;;;; part of a word) which returns two values. If the word part shouldn't be
;;;; learned by the markov chain specified by the spec, the two values should be
;;;; NIL. If the markov spec can learn something from the given word part, the
;;;; first return value is the distribution to be learned and the second value a
;;;; predicate on two values. The first parameter of the predicate is the word
;;;; generated so far, the second parameter the size of the distribution
;;;; collected so far from other markov chains in the same run.
(with-memoization
  (defun parse-simple-markov-spec (glyphs categories)
    (declare (type map categories))
    (>>!
      katz (<? (>>!
                 katz (parse-number)
                 _ (>> (parse-constant ">")
                       (parse-whitespace-no-newline))
                 (succeed katz))
               0)
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
        (assert (> outro 0))
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
                                (values (uniform-distribution (set outro))
                                        (memoized
                                         (intro filter)
                                         (lambda (word dist-size)
                                           (and (<= dist-size katz)
                                                (postfix? intro
                                                          (filter filter
                                                                  word)))))))
                              (values nil nil)))
                        (values nil nil)))))))))

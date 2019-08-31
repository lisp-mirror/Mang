(in-package #:mang)

;;;;; everything :=
;;;;;  long     = 4:1,
;;;;;  middle   = 3:1,
;;;;;  short    = 2:1,
;;;;;  support  = 1:1,
;;;;;  fallback = 0:1,
;;;;;  C-long   = 4:1~C,
;;;;;  C-middle = 3:1~C,
;;;;;  C-short  = 2:1~C,
;;;;;  V-long   = 3:1~V,
;;;;;  V-middle = 2:1~V,
;;;;;  V-short  = 1:1~V
;;;;;  | (3 C-long   + 2 [1000 long  ])
;;;;;  + (3 V-long   + 2 [1000 long  ])
;;;;;  + (2 C-middle +   [ 500 middle])
;;;;;  + (2 V-middle +   [ 500 middle])
;;;;;  + (  C-short  +   [ 200 short ])
;;;;;  + (2 V-short  +   [ 200 short ])
;;;;;  + ([1000 support ] + 2 [500 support ])
;;;;;  + ([ 750 fallback] +   [500 fallback])
;;;; The part before the `|` defines the available markov chains, the part after
;;;; defines the way they are used in the generator.
;;;; The [n mc] construction loads the markov chain `mc` only when the current
;;;; distribution under construction has a size lower than or equal to `n`.
;;;; `()` locally construct a distribution.
;;;; `+` adds distributions.

;;;; A markov spec returns a function taking one argument (which is an initial
;;;; part of a word) which returns two values. If the word part shouldn't be
;;;; learned by the markov chain specified by the spec, the two values should be
;;;; NIL. If the markov spec can learn something from the given word part, the
;;;; first return value is the distribution to be learned and the second value a
;;;; predicate on one value. The parameter of the predicate is the word
;;;; generated so far.
(with-memoization
  (defun parse-markov-spec (glyphs categories)
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
                                         (lambda (word)
                                           (postfix? intro
                                                     (filter filter
                                                             word))))))
                              (values nil nil)))
                        (values nil nil)))))))))

(defun parse-markov-definition (glyphs categories)
  (>>!
    name (parse-identifier)
    _ (>> (parse-whitespace-no-newline)
          (parse-constant "=")
          (parse-whitespace-no-newline))
    markov-spec (parse-markov-spec glyphs categories)
    (succeed (map (name markov-spec)
                  :default (empty-set)))))

(defun parse-markov-definitions (glyphs categories)
  (parse-separated (parse-markov-definition glyphs categories)
                   ","
                   (empty-map (empty-set))
                   (lambda (spec specs)
                     (map-union specs spec #'union))))

(defun parse-markov-gen-spec (markov-definitions)
  (declare (type map markov-definitions))
  (<$> (parse-separated (// (<$> (parse-from-map markov-definitions)
                                 #'first)
                            (>>!
                              mult (parse-number)
                              _ (parse-whitespace-no-newline)
                              spec (parse-markov-gen-spec markov-definitions)
                              (succeed `(:mult ,mult ,spec)))
                            (>>!
                              _ (>> (parse-constant "[")
                                    (parse-whitespace))
                              cutoff (parse-number)
                              _ (parse-whitespace)
                              spec (parse-markov-gen-spec markov-definitions)
                              (succeed `(:katz ,cutoff ,spec)))
                            (>>!
                              _ (>> (parse-constant "(")
                                    (parse-whitespace))
                              spec (parse-markov-gen-spec markov-definitions)
                              _ (>> (parse-whitespace)
                                    (parse-constant ")"))
                              (succeed spec)))
                        "+")
       (lambda (spec)
         `(:sequence ,@spec))))

(defun parse-markov (glyphs categories)
  (declare (type map glyphs categories))
  (>>!
    name (parse-identifier)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    markov-definitions (parse-markov-definitions glyphs categories)
    _ (>> (parse-whitespace)
          (parse-constant "|")
          (parse-whitespace))
    gen-spec (parse-markov-gen-spec markov-definitions)
    (succeed (map (name `(,markov-definitions ,gen-spec))))))

(defun parse-markovs (glyphs categories)
  (parse-lines (parse-markov glyphs categories)
               (empty-map)
               (lambda (markov markovs)
                 (map-union markovs markov))))

(defun parse-markov-section (glyphs categories)
  (parse-section "markovs" (parse-markovs glyphs categories)))

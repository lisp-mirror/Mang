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
;;;; part of a word) which returns a map or nil, if the word part shouldn't be
;;;; learned. If the markov spec can learn something from the given word part,
;;;; the return value is a map from predicates to probability distributions. The
;;;; parameter of the predicate is the word generated so far.
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
                    (declare (type list word))
                    (if (length>= word match-length)
                        (bind ((rev-word (reverse word))
                               (rev-init (filter filter
                                                 (subseq rev-word outro))))
                          (if (length>= rev-init intro)
                              (bind ((intro (reverse (subseq rev-init 0 intro)))
                                     (outro (reverse (subseq rev-word 0
                                                             outro))))
                                (map ((memoized
                                       (intro filter)
                                       (lambda (word)
                                         (declare (type list word))
                                         (postfix? intro
                                                   (filter filter
                                                           word))))
                                      (uniform-distribution (set outro)))
                                     :default <nodist>))
                              (empty-map <nodist>)))
                        (empty-map <nodist>)))))))))

(defun parse-markov-definition (glyphs categories)
  (declare (type map glyphs categories))
  (>>!
    name (parse-identifier)
    _ (>> (parse-whitespace-no-newline)
          (parse-constant "=")
          (parse-whitespace-no-newline))
    markov-spec (parse-markov-spec glyphs categories)
    (succeed (map (name markov-spec)
                  :default (empty-set)))))

(defun parse-markov-definitions (glyphs categories)
  (declare (type map glyphs categories))
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
  (declare (type map glyphs categories))
  (parse-lines (parse-markov glyphs categories)
               (empty-map)
               (lambda (markov markovs)
                 (map-union markovs markov))))

(defun parse-markov-section (glyphs categories)
  (declare (type map glyphs categories))
  (parse-section "markovs" (parse-markovs glyphs categories)))

(defun learn-markov (markov spec word)
  (declare (type map markov)
           (type function spec)
           (type list word))
  (loop :for length :below (length word)
     :do (setf markov
               (map-union markov
                          (funcall spec (subseq word 0 length))
                          #'union))
     :finally (return-from learn-markov
                markov)))

(defun learn (store markov-spec word categories)
  (declare (type map store markov-spec)
           (type list word)
           (type set categories))
  (convert 'map
           categories
           :value-fn #'identity
           :key-fn
           (lambda (category)
             (image (lambda (name markov)
                      (values name (learn-markov markov (@ markov-spec name)
                                                 word)))
                    (@ store category)))))

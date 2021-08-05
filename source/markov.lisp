(in-package #:mang)

;;;;; everything :=
;;;;;  long      = 4,
;;;;;  middle    = 3,
;;;;;  short     = 2,
;;;;;  lsupport  = 1:2,
;;;;;  support   = 1,
;;;;;  fallback  = 0,
;;;;;  C-long    = 4~C,
;;;;;  C-middle  = 3~C,
;;;;;  C-short   = 2~C,
;;;;;  V-long    = 3~V,
;;;;;  V-middle  = 2~V,
;;;;;  V-short   = 1~V,
;;;;;  C-uniform = uniform {m,p,f,n,t,s,k,x,l,r} 2,
;;;;;  C-Zipf    = zipf    {m,p,f,n,t,s,k,x,l,r} 1.06 3,
;;;;;  V-uniform = uniform {i,a,u,e,o} 4,
;;;;;  V-Zipf    = zipf    {i,a,u,e,o} 1.09 5,
;;;;;  end       = uniform {#}
;;;;;  | {(3    C-long)   + (2 [1000 long   ])}
;;;;;  + {(3    V-long)   + (2 [1000 long   ])}
;;;;;  + {(2    C-middle) +    [ 500 middle ] }
;;;;;  + {(2    V-middle) +    [ 500 middle ] }
;;;;;  + {      C-short   +    [ 200 short  ] }
;;;;;  + {(2    V-short)  +    [ 200 short  ] }
;;;;;  + [1000 lsupport] + (2 [500 support])
;;;;;  + [ 750 fallback] + [500 fallback]
;;;;;  + [10 (4 end)] + [100 (3 end)] + [1000 (2 end)] + [10000 end]
;;;;;  + C-uniform + (2 C-Zipf)
;;;;;  + V-uniform + (2 V-Zipf) + end
;;;; The part before the `|` defines the available markov chains, the part after
;;;; defines the way they are used in the generator.
;;;; The (n mc) construction multiplies the weight of the markov chain `mc` by a
;;;; factor of `n`.
;;;; The [n mc] construction loads the markov chain `mc` only when the current
;;;; distribution under construction has a size lower than or equal to `n`.
;;;; `{}` locally constructs a distribution. This is only of relevance to katz
;;;; backoff.
;;;; `+` adds distributions.

;;;; A markov spec returns a function taking one argument (which is an initial
;;;; part of a word) which returns a map or nil, if the word part shouldn't be
;;;; learned. If the markov spec can learn something from the given word part,
;;;; the return value is a map from predicates to probability distributions. The
;;;; parameter of the predicate is the word generated so far.
(with-memoization
  (defun parse-markov-spec (phonemes categories)
    (declare (type set phonemes)
             (type map categories))
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
                 (with (image #'list
                              phonemes)
                       (map (:begin t))))
      (bind ((match-length (+ intro outro)))
        (declare (type (integer 0)
                       match-length)
                 (type set filter))
        (assert (> outro 0))
        (locally
            (declare (type (integer 0)
                           intro)
                     (type (integer 1)
                           outro))
          (succeed `(,(set
                       (lambda (word)
                         (declare (type list word))
                         (if (length>= word match-length)
                             (bind ((rev-word (reverse word))
                                    (rev-init (filter filter
                                                      (subseq rev-word outro))))
                               (declare (type list rev-word rev-init))
                               (if (length>= rev-init intro)
                                   (bind ((intro
                                           (reverse (subseq rev-init 0 intro)))
                                          (outro (reverse (subseq rev-word 0
                                                                  outro))))
                                     (declare (type list intro outro))
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
                             (empty-map <nodist>))))
                     ,(empty-map <nodist>))))))))

(defun parse-uniform-spec (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-constant "uniform")
          (parse-whitespace))
    elements (parse-wrapped "{"
                            (parse-separated (// (<$> (parse-from-map glyphs)
                                                      #'second)
                                                 (<$ (parse-constant "#")
                                                     (map (:end t))))
                                             ",")
                            "}")
    weight (<? (>> (parse-whitespace)
                   (parse-number))
               1)
    (succeed `(,(empty-set)
                ,(map (#'true (uniform-distribution elements weight))
                      :default <nodist>)))))

(defun parse-zipf-spec (glyphs)
  (declare (type map glyphs))
  (>>!
    _ (>> (parse-constant "zipf")
          (parse-whitespace))
    elements (parse-wrapped "{"
                            (parse-separated (// (<$> (parse-from-map glyphs)
                                                      #'second)
                                                 (<$ (parse-constant "#")
                                                     (map (:end t))))
                                             ",")
                            "}")
    exponent (<? (>> (parse-whitespace)
                     (parse-floating))
                 1)
    weight (<? (>> (parse-whitespace)
                   (parse-number))
               1)
    (succeed `(,(empty-set)
                ,(map (#'true (zipf-distribution elements exponent weight))
                      :default <nodist>)))))
;;; -> (Set (Word -> Map (Word -> Bool) (Dist Word)),
;;;     Map (Word -> Bool) (Dist Word))

(defun parse-markov-definition (glyphs categories)
  (declare (type map glyphs categories))
  (>>!
    name (parse-identifier)
    _ (>> (parse-whitespace-no-newline)
          (parse-constant "=")
          (parse-whitespace-no-newline))
    (markov-spec store)
    (// (parse-markov-spec (range glyphs)
                           categories)
        (parse-uniform-spec glyphs)
        (parse-zipf-spec glyphs))
    (locally
        (declare (type string name)
                 (type set markov-spec))
      (succeed `(,(map (name markov-spec)
                       :default (empty-set))
                  ,(map (name store)))))))
;;; -> (Map Name (Set (Word -> Map (Word -> Bool) (Dist Word))),
;;;     Map Name (Map (Word -> Bool) (Dist Word)))

(defun parse-markov-definitions (glyphs categories)
  (declare (type map glyphs categories))
  (parse-separated (parse-markov-definition glyphs categories)
                   ","
                   `(,(empty-map (empty-set))
                      ,(empty-map (empty-map <nodist>)))
                   (lambda (def defs)
                     (bind (((specs stores)
                             defs)
                            ((spec store)
                             def))
                       `(,(deep-union specs spec)
                          ,(deep-union stores store))))))
;;; -> (Map Name (Set (Word -> Map (Word -> Bool) (Dist Word))),
;;;     Map Name (Map (Word -> Bool) (Dist Word)))

(defun parse-markov-gen-spec (markov-definitions)
  (declare (type map markov-definitions))
  (<$> (parse-separated (//_
                            (<$> (parse-from-map markov-definitions)
                                 #'first)
                          (parse-wrapped "(" (>>!
                                               mult (parse-number)
                                               _ (parse-whitespace-no-newline)
                                               spec (parse-markov-gen-spec
                                                     markov-definitions)
                                               (succeed `(:mult ,mult ,spec)))
                                         ")")
                          (parse-wrapped "[" (>>!
                                               cutoff (parse-number)
                                               _ (parse-whitespace)
                                               spec (parse-markov-gen-spec
                                                     markov-definitions)
                                               (succeed `(:katz ,cutoff ,spec)))
                                         "]")
                          (<$> (parse-wrapped "{" (parse-markov-gen-spec
                                                   markov-definitions)
                                              "}")
                               (lambda (spec)
                                 `(:descend ,spec))))
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
    (markov-definitions store)
    (parse-markov-definitions glyphs categories)
    _ (>> (parse-whitespace)
          (parse-constant "|")
          (parse-whitespace))
    gen-spec (parse-markov-gen-spec markov-definitions)
    (succeed `(,(map (name `(,markov-definitions ,gen-spec))
                     :default (empty-map (empty-set)))
                ,(map (name store)
                      :default (empty-map (empty-map <nodist>)))))))
;;; -> (Map Name (Map Name (Set (Word -> Map (Word -> Bool) (Dist Word)))),
;;;     Map Name (Map Name (Map (Word -> Bool) (Dist Word))))

(defun parse-markov-section (glyphs categories)
  (declare (type map glyphs categories))
  (parse-section "markovs"
                 (parse-lines (parse-markov glyphs categories)
                              `(,(empty-map (empty-map (empty-set)))
                                 ,(empty-map (empty-map (empty-map <nodist>))))
                              (lambda (markov markovs)
                                (bind (((markovs stores)
                                        markovs)
                                       ((markov store)
                                        markov))
                                  `(,(deep-union markovs markov)
                                     ,(deep-union stores store)))))))

(defun empty-store ()
  ;; category part       predicate distribution
  (empty-map (empty-map (empty-map <nodist>))))

(defun store-from-word (word markov-spec categories)
  (declare (type list word)
           (type map markov-spec)
           (type set categories))
  (bind ((markov-spec (keep categories markov-spec))
         (intros (intros word)))
    (image (lambda (category parts)
             (values
              category
              (image (lambda (part functions)
                       (values part
                               (reduce (lambda (acc function)
                                         (reduce (lambda (acc intro)
                                                   (deep-union (funcall function
                                                                        intro)
                                                               acc))
                                                 intros
                                                 :initial-value acc))
                                       functions
                                       :initial-value (empty-map <nodist>))))
                     (first parts))))
           (with-default markov-spec
             (empty-map (empty-map <nodist>))))))

(defun learn (store markov-spec word categories)
  (declare (type map store markov-spec)
           (type list word)
           (type set categories))
  (deep-union store (store-from-word word markov-spec categories)))

(defun unlearn (store markov-spec word categories)
  (declare (type map store markov-spec)
           (type list word)
           (type set categories))
  (bind ((word-store (store-from-word word markov-spec categories)))
    (image (lambda (category parts)
             (values category
                     (image (lambda (part functions)
                              (values part
                                      (image (lambda (function distribution)
                                               (values function
                                                       (distribution-minus
                                                        distribution
                                                        (@ (@ (@ word-store
                                                                 category)
                                                              part)
                                                           function))))
                                             functions)))
                            parts)))
           store)))

(defun dist-from-markov (word store markov-spec dfsm state categories
                         &optional (negative-categories (empty-set)))
  (declare (type list word)
           (type map store markov-spec)
           (type dfsm dfsm)
           (type symbol state)
           (type set categories negative-categories))
  (labels ((_extract-dist (markovs gen-spec dist &optional (size 0))
             (if (consp gen-spec)
                 (bind (((type &rest args)
                         gen-spec))
                   (ecase type
                     (:sequence
                      (if args
                          (chop curr args
                            (bind ((new-dist (_extract-dist markovs curr <nodist>
                                                            size)))
                              (_extract-dist markovs `(:sequence ,@args)
                                             (union dist new-dist)
                                             (+ size (size new-dist)))))
                          dist))
                     (:mult
                      (bind (((factor gen-spec)
                              args))
                        (union (mult (_extract-dist markovs gen-spec <nodist>
                                                    size)
                                     factor)
                               dist)))
                     (:katz
                      (bind (((cutoff gen-spec)
                              args))
                        (if (<= size cutoff)
                            (_extract-dist markovs gen-spec dist size)
                            dist)))
                     (:descend
                      (_extract-dist markovs (first args)
                                     dist 0))))
                 (reduce #'union
                         (convert 'set
                                  (@ markovs gen-spec)
                                  :pair-fn
                                  (lambda (predicate dist)
                                    (if (funcall predicate word)
                                        (keep #'second
                                              (image (lambda (element)
                                                       `(,element
                                                         ,(advance-dfsm dfsm
                                                                        state
                                                                        element)))
                                                     dist))
                                        <nodist>)))
                         :initial-value <nodist>)))
           (_get-dist (categories)
             (reduce (lambda (dist category)
                       (union (normalize (_extract-dist (@ store category)
                                                        (second (@ markov-spec
                                                                   category))
                                                        <nodist>))
                              dist))
                     categories
                     :initial-value <nodist>)))
    (diminish (_get-dist categories)
              (_get-dist negative-categories))))

(defun generate-next (word dfsm state store markov-spec categories
                      &optional (negative-categories (empty-set)))
  (declare (type dfsm dfsm)
           (type map store markov-spec)
           (type set categories negative-categories))
  (bind ((transition-table (transitions<- dfsm))
         (transitions (domain (@ transition-table state))))
    (cond
      ((@ (accepting-states<- dfsm)
          state)
       (values t nil))
      ((empty? transitions)
       (values nil nil))
      (t
       (bind (((transition state)
               (extract-random (dist-from-markov word store markov-spec dfsm
                                                 state categories
                                                 negative-categories))))
         (values (ensure-list transition)
                 state))))))

(defun generate-word (dfsm store markov-spec categories
                      &optional (negative-categories (empty-set)))
  (declare (type dfsm dfsm)
           (type map store markov-spec)
           (type set categories negative-categories))
  (bind ((word `(,(map (:begin t))))
         (state (start-state<- dfsm)))
    (declare (type (cons map list)
                   word))
    (loop
       (bind (((:values outro new-state)
               (generate-next word dfsm state store markov-spec categories
                              negative-categories)))
         (declare (type (or list (eql t))
                        outro))
         (if new-state
             (locally
                 (declare (type cons outro))
               (setf word (append word outro)
                     state new-state))
             (locally
                 (declare (type boolean outro))
               (return-from generate-word
                 (values word outro))))))))

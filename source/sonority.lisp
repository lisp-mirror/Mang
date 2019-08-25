(in-package #:mang)

(defmethod before? (a b (sequence cons))
  (labels ((_find (x xs)
             (when xs
               (bind (((f &rest r)
                       xs))
                 (if (typecase f
                       (set
                        (@ f x))
                       (function
                        (funcall f x))
                       (t
                        (equal? f x)))
                     (values r t)
                     (_find x r))))))
    (nth-value 1 (_find b (_find a sequence)))))

(defmethod nuclei ((word cons)
                   (hierarchy cons))
  (labels ((_rec (n l acc)
             (if (rest l)
                 (bind (((p c &rest r)
                         l))
                   (cond
                     ((= n 0)
                      (_rec 1 l (if (before? p c hierarchy)
                                    acc
                                    (cons 0 acc))))
                     (r
                      (_rec (1+ n)
                            (cons c r)
                            (if (or (before? c p hierarchy)
                                    (before? c (first r)
                                             hierarchy))
                                acc
                                (cons n acc))))
                     (t
                      (if (before? c p hierarchy)
                          (nreverse acc)
                          (nreverse (cons n acc))))))
                 (list n))))
    (_rec 0 word '())))

(defmethod syllable-init ((word cons)
                          (nucleus-index integer)
                          (earliest-start integer)
                          (hierarchy cons))
  (or (loop :for x :downfrom (1- nucleus-index)
         :for y :from (- nucleus-index 2)
         :downto earliest-start
         :unless (before? (nth y word)
                          (nth x word)
                          hierarchy)
         :do (return-from syllable-init
               y))
      earliest-start))

(defmethod syllable-coda ((word cons)
                          (nucleus-index integer)
                          (latest-end integer)
                          (hierarchy cons))
  (or (loop :for x :from (1+ nucleus-index)
         :for y :from (+ nucleus-index 2)
         :to latest-end
         :unless (before? (nth y word)
                          (nth x word)
                          hierarchy)
         :do (return-from syllable-coda
               y))
      latest-end))

(defmethod syllable-inits ((word cons)
                           (hierarchy cons))
  (bind ((nuclei (nuclei word hierarchy)))
    (cons 0 (loop :for (prev this)
               :on nuclei
               :if this
               :collect (1+ (syllable-init word this prev hierarchy))))))

(defmethod syllable-codas ((word cons)
                           (hierarchy cons))
  (bind ((nuclei (nuclei word hierarchy)))
    (loop :for (this next)
       :on nuclei
       :if next
       :collect (syllable-coda word this next hierarchy))))

(defmethod syllabalize ((word cons)
                        (hierarchy cons)
                        &optional (prefer-open? t))
  (declare (type boolean prefer-open?))
  (apply #'append
         (intersperse '("")
                      (if prefer-open?
                          (bind ((inits (syllable-inits word hierarchy)))
                            (loop :for (index next) :on inits
                               :collect
                               (subseq word index next)))
                          (bind ((codas (cons 0 (syllable-codas word
                                                                hierarchy))))
                            (loop :for (prev index) :on codas
                               :collect
                               (subseq word prev index)))))))

(defmethod resyllabalize ((word cons)
                          (hierarchy cons)
                          &optional (prefer-open? t))
  (declare (type boolean prefer-open?))
  (syllabalize (remove "" word
                       :test #'string=)
               hierarchy prefer-open?))

(defun parse-sonority-class (glyphs categories)
  (declare (type map glyphs categories))
  (parse-separated (<$> (// (parse-from-map categories)
                            (parse-from-map glyphs))
                        #'second)
                   "," (empty-set)
                   (lambda (part parts)
                     (with parts part))))

(defun parse-sonority-hierarchy (glyphs categories)
  (declare (type map glyphs categories))
  (parse-subsection "sonority-hierarchy"
                    (parse-lines (parse-sonority-class glyphs categories))))

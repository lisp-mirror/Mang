(in-package #:mang)

(defmethod before? (a b (sequence cons))
  (destructuring-bind (f &rest r)
      sequence
    (if (typep f 'set)
        (cond
          ((@ f a)
           (not (@ f b)))
          ((@ f b)
           nil)
          (t
           (before? a b r)))
        (cond
          ((equal? f a)
           t)
          ((equal? f b)
           nil)
          (t
           (before? a b r))))))

(defmethod indices (predicate (sequence null))
  '())

(defmethod indices ((predicate t)
                    (sequence cons))
  (labels ((_rec (s i inds)
             (if s
                 (destructuring-bind (f &rest r)
                     s
                   (if (equal? predicate f)
                       (_rec r (1+ i)
                             (cons i inds))
                       (_rec r (1+ i)
                             inds)))
                 (nreverse inds))))
    (_rec sequence 0 '())))

(defmethod indices ((predicate set)
                    (sequence cons))
  (labels ((_rec (s i inds)
             (if s
                 (destructuring-bind (f &rest r)
                     s
                   (if (@ predicate f)
                       (_rec r (1+ i)
                             (cons i inds))
                       (_rec r (1+ i)
                             inds)))
                 (nreverse inds))))
    (_rec sequence 0 '())))

(defmethod indices ((predicate function)
                    (sequence cons))
  (labels ((_rec (s i inds)
             (if s
                 (destructuring-bind (f &rest r)
                     s
                   (if (funcall predicate f)
                       (_rec r (1+ i)
                             (cons i inds))
                       (_rec r (1+ i)
                             inds)))
                 (nreverse inds))))
    (_rec sequence 0 '())))

(defmethod syllable-init ((word cons)
                          (nucleus-index integer)
                          (earliest-start integer)
                          (hierarchy cons))
  (or (loop :for x :from (1- nucleus-index)
         :for y :from (- nucleus-index 2)
         :downto earliest-start
         :unless (before? (nth x word)
                          (nth y word)
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
         :unless (before? (nth x word)
                          (nth y word)
                          hierarchy)
         :do (return-from syllable-coda
               y))
      latest-end))

(defmethod syllable-inits ((word cons)
                           (vowels set)
                           (hierarchy cons))
  (let ((nuclei (indices vowels word)))
    (cons 0 (loop :for (prev this)
               :on nuclei
               :if this
               :collect (syllable-init word this prev hierarchy)))))

(defmethod syllable-codas ((word cons)
                           (vowels set)
                           (hierarchy cons))
  (let ((nuclei (indices vowels word)))
    (loop :for (this next)
       :on nuclei
       :if next
       :collect (1+ (syllable-coda word this next hierarchy)))))

(defmethod syllabalize ((word cons)
                        (vowels set)
                        (hierarchy cons)
                        prefer-open?)
  (declare (type boolean prefer-open?))
  (apply #'append
         (intersperse '("")
                      (if prefer-open?
                          (let ((inits (syllable-inits word vowels hierarchy)))
                            (loop :for (index next) :on inits
                               :collect
                               (subseq word index next)))
                          (let ((codas (cons 0 (syllable-codas word vowels hierarchy))))
                            (loop :for (prev index) :on codas
                               :collect
                               (subseq word prev index)))))))

(defmethod resyllabalize ((word cons)
                          (vowels set)
                          (hierarchy cons)
                          prefer-open?)
  (declare (type boolean prefer-open?))
  (syllabalize (remove "" word
                       :test #'string=)
               vowels hierarchy prefer-open?))

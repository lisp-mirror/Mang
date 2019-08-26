(in-package #:mang)

(defun syllable-nuclei (word hierarchy)
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

(defun syllable-init (word nucleus-index earliest-start hierarchy)
  (or (loop :for x :downfrom (1- nucleus-index)
         :for y :from (- nucleus-index 2)
         :downto earliest-start
         :unless (before? (nth y word)
                          (nth x word)
                          hierarchy)
         :do (return-from syllable-init
               y))
      earliest-start))

(defun syllable-coda (word nucleus-index latest-end hierarchy)
  (or (loop :for x :from (1+ nucleus-index)
         :for y :from (+ nucleus-index 2)
         :to latest-end
         :unless (before? (nth y word)
                          (nth x word)
                          hierarchy)
         :do (return-from syllable-coda
               y))
      latest-end))

(defun syllable-inits (word hierarchy)
  (bind ((nuclei (nuclei word hierarchy)))
    (cons 0 (loop :for (prev this)
               :on nuclei
               :if this
               :collect (1+ (syllable-init word this prev hierarchy))))))

(defun syllable-codas (word hierarchy)
  (bind ((nuclei (nuclei word hierarchy)))
    (loop :for (this next)
       :on nuclei
       :if next
       :collect (syllable-coda word this next hierarchy))))

(defun syllabalize (word hierarchy
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

(defun resyllabalize (word hierarchy
                      &optional (prefer-open? t))
  (declare (type boolean prefer-open?))
  (syllabalize (remove "" word
                       :test #'string=)
               hierarchy prefer-open?))

(defun parse-sonority-class (glyphs categories)
  (declare (type map glyphs categories))
  (>>!
    _ (parse-whitespace)
    front (// (parse-from-map categories)
              (parse-from-map glyphs))
    back (many (>> (parse-whitespace)
                   (parse-constant ",")
                   (parse-whitespace)
                   (// (parse-from-map categories)
                       (parse-from-map glyphs)))
               (empty-set)
               (lambda (phoneme phonemes)
                 (with phonemes phoneme)))
    _ (parse-expression-end)
    (succeed (with back front))))

(defun parse-sonority-hierarchy (glyphs categories)
  (declare (type map glyphs categories))
  (>> (parse-whitespace)
      (parse-constant "#sonority-hierarchy:")
      (parse-expression-end)
      (some (parse-sonority-class glyphs categories)
            '() #'cons)))

(in-package #:mang)

(defun fst-filter (predicate)
  (declare (type function predicate))
  (fst-elementary predicate '()
                  :consume? nil))

(defun fst-emit (generator)
  (declare (type function generator))
  (fst-elementary #'true generator
                  :consume? nil))

(defun fst-compare-phoneme (phoneme)
  (declare (type map phoneme))
  (fst-filter (lambda (other-phoneme)
                (declare (type map other-phoneme))
                (equal? phoneme other-phoneme?))))

(defun fst-compare-features (constant present absent)
  (declare (type map constant)
           (type set present absent))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (has-features? phoneme constant present absent))))

(defun fst-compare-register (register)
  (declare (special *phoneme-registry*))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (has-features? phoneme (gethash register *phoneme-registry*
                                                (empty-map))
                               (empty-set)
                               (empty-set)))))

(defun fst-emit-phoneme (phoneme
                         &key
                           (constant-supplement (empty-map))
                           (present-supplement (empty-set))
                           (absent-supplement (empty-set))
                           (register-supplement (empty-map)))
  (declare (special *phoneme-registry*)
           (type map phoneme constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-emit
   (lambda (glyph)
     (declare (ignore glyph))
     (list (augment-feature-set
            phoneme
            (map-union constant-supplement
                       (image (lambda (feature register)
                                (values feature
                                        (@ (gethash register *phoneme-registry*)
                                           feature)))
                              register-supplement))
            present-supplement absent-supplement)))))

(defun fst-emit-register (register
                          &key
                            (constant-supplement (empty-map))
                            (present-supplement (empty-set))
                            (absent-supplement (empty-set))
                            (register-supplement (empty-map)))
  (declare (special *phoneme-registry*)
           (type map constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-emit
   (lambda (glyph)
     (declare (ignore glyph))
     (list (augment-feature-set
            (gethash register *phoneme-registry*)
            (map-union constant-supplement
                       (image (lambda (feature register)
                                (values feature
                                        (@ (gethash register *phoneme-registry*)
                                           feature)))
                              register-supplement))
            present-supplement absent-supplement)))))

(defun fst-emit-category (category register
                          &key
                            (constant-supplement (empty-map))
                            (present-supplement (empty-set))
                            (absent-supplement (empty-set))
                            (register-supplement (empty-map)))
  (declare (special *category-registry* *phoneme-registry*)
           (type map category constant-supplement register-supplement)
           (type set present-supplement absent-supplement))
  (fst-elementary
   #'true
   (lambda (glyph)
     (declare (ignore glyph))
     (list
      (map-union (nth (gethash register *category-registry*)
                      category)
                 (map-union constant-supplement
                            (image (lambda (feature register)
                                     (values feature
                                             (@ (gethash register
                                                         *phoneme-registry*)
                                                feature)))
                                   register-supplement)))))
   :consume? nil))

(defun fst-write-features (features)
  (declare (special *phoneme-registry*)
           (type map features))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (dolist (feature (convert 'list features)
                         t)
                  (bind (((feature register)
                          feature))
                    (setf (gethash register *phoneme-registry*)
                          (with (gethash register *phoneme-registry*)
                                feature (@ phoneme feature))))))))
/
(defun fst-write-register (register)
  (declare (special *phoneme-registry*))
  (fst-elementary (lambda (phoneme)
                    (declare (type map phoneme))
                    (setf (gethash register *phoneme-registry*)
                          phoneme)
                    t)))

(defun fst-write-category (category register)
  (declare (special *category-registry* *phoneme-registry*)
           (type map category))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                ([a]when (in-category? phoneme category)
                  (setf (gethash register *category-registry*)
                        it
                        (gethash register *phoneme-registry*)
                        phoneme)
                  t))))

(defun check-category (category)
  (declare (type category))
  (fst-filter (lambda (phoneme)
                (declare (type map phoneme))
                (in-category? phoneme category))))

(defun parse-glyphs-comp (glyphs)
  (declare (type map glyphs))
  (some (>> (parse-whitespace-no-newline)
            (parse-glyph glyphs))
        (empty-fst)
        (lambda (glyph filter)
          (bind (((_ phoneme)
                  glyph))
            (fst-sequence (fst-compare-phoneme phoneme)
                          filter)))))

(defun parse-glyphs-emit (glyphs)
  (declare (type map glyphs))
  (some (>> (parse-whitespace-no-newline)
            (parse-glyph glyphs))
        (empty-fst)
        (lambda (glyph emitter)
          (bind (((_ phoneme)
                  glyph))
            (fst-sequence (fst-emit-phoneme phoneme)
                          emitter)))))

(defun parse-glyphs-comp-emit (glyphs)
  (declare (type map glyph))
  (some (>> (parse-whitespace-no-newline)
            (parse-glyph glyphs))
        `(,(empty-fst)
           ,(empty-fst))
        (lambda (glyph filter/emitter)
          (bind (((_ phoneme)
                  glyph)
                 ((filter emitter)
                  filter/emitter))
            `(,(fst-sequence (fst-compare-phoneme phoneme)
                             filter)
               ,(fst-sequence (fst-emit-phoneme phoneme)
                              emitter))))))

(defun parse-register ()
  (// (parse-number)
      (parse-wrapped "{" (parse-identifier *mang-reserved-symbols*)
                     "}")))

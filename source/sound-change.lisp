(in-package #:mang)

(defun fst-filter (predicate)
  (fst-elementary predicate '()
                  :consume? nil))

(defun fst-emit (generator)
  (fst-elementary #'true generator
                  :consume? nil))

(defun fst-compare-features (constant present absent)
  (fst-filter (lambda (phoneme)
                (has-features? phoneme constant present absent))))

(defun fst-compare-register (register)
  (declare (special *phoneme-registry*))
  (fst-filter (lambda (phoneme)
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
  (declare (special *phoneme-registry*))
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
  (declare (special *phoneme-registry*))
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
  (declare (special *category-registry* *phoneme-registry*))
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
  (declare (special *phoneme-registry*))
  (fst-filter (lambda (phoneme)
                (dolist (feature (convert 'list features)
                         t)
                  (bind (((feature register)
                          feature))
                    (setf (gethash register *phoneme-registry*)
                          (with (gethash register *phoneme-registry*)
                                feature (@ phoneme feature))))))))

(defun fst-write-register (register)
  (declare (special *phoneme-registry*))
  (fst-elementary (lambda (phoneme)
                    (setf (gethash register *phoneme-registry*)
                          phoneme)
                    t)))

(defun fst-write-category (category register)
  (declare (special *category-registry* *phoneme-registry*))
  (fst-filter (lambda (phoneme)
                ([a]when (in-category? phoneme category)
                  (setf (gethash register *category-registry*)
                        it
                        (gethash register *phoneme-registry*)
                        phoneme)
                  t))))

(defun check-category (category)
  (fst-filter (lambda (phoneme)
                (in-category? phoneme category))))

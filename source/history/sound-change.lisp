(in-package #:mang)

;;; .[+obstruent,-voice]1V2/.1[front2]V[round3]2/V3_

(defmethod apply-sound-change ((word word)
                               (sound-change fst))
  (bind ((*phoneme-registry* (make-hash-table :test 'equal))
         (*category-registry* (make-hash-table :test 'equal)))
    (declare (special *phoneme-registry* *category-registry*)
             (type hash-table *phoneme-registry* *category-registry*))
    (image (lambda (solution)
             (word (mapcar #'funcall
                           solution)
                   :origin word
                   :transformations (transformations<- word)))
           (run-fst sound-change (form<- word)))))

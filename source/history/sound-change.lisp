(in-package #:mang)

(defun save-glyph (condition)
  (declare (special *registry*))
  (let ((register (gensym "register")))
    (values (lambda (glyph)
              (when (funcall condition glyph)
                (setf (gethash register *registry*)
                      glyph)
                t))
            register)))

(defun load-register (register)
  (declare (special *registry*))
  (lambda ()
    (gethash register *registry*)))

(defun load-category-replace (register source target)
  (declare (special *registry*))
  (lambda ()
    (elt target (position (gethash register *registry*)
                          source))))

(defun load-replace-feature (register feature value)
  (declare (special *registry*))
  (lambda ()
    (with (gethash register *registry*)
          feature value)))

(defun load-blend-features (base-register supplement-register features)
  (declare (special *registry*))
  (lambda ()
    (map-union (gethash base-register *registry*)
               (filter features (gethash supplement-register *registry*)))))

(defun sound-change (to-replace replacement pre post)
  (declare (special *registry*))
  (let ((start (gensym "start-sound-change"))
        (finish (gensym "finish-sound-change"))
        (inner1 (gensym "sound-change-inner-in"))
        (inner2 (gensym "sound-change-inner-out")))
    (modify-fst
     (add-epsilon-transitions
      (fst-preferred
       (build-sca-fst to-replace replacement pre post)
       (multiple-value-bind (condition register)
           (save-glyph (constantly t))
         (fst-elementary condition (load-register register)))
       :in-state inner1
       :out-state inner2)
      start finish
      start inner1
      inner2 inner1
      inner2 finish))))

(defmethod apply-sound-change ((word word)
                               (sound-change fst))
  (let ((*registry* (make-hash-table :test 'eq)))
    (declare (special *registry*))
    (word (mapcar #'funcall
                  (run-fst sound-change (form<- word))
                  :origin word
                  :transformations (transformations<- word)))))

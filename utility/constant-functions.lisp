(in-package #:mang)

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun empty (&rest args)
  (declare (ignore args))
  ())

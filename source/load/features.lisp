(in-package #:mang)

(defun parse-feature-section ()
  (parse-section
   "feature"
   (parse-lines (parse-feature-definition)
                `(,(empty-set)
                   ,(empty-map)
                   ,(empty-set))
                (lambda (def defs)
                  (bind (((binary valued privative)
                          defs)
                         ((nbinary nvalued nprivative)
                          def))
                    `(,(union binary nbinary)
                       ,(map-union valued nvalued)
                       ,(union privative nprivative)))))))

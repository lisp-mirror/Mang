(in-package #:mang)

(defgeneric write-dot (stream graph
                       &key
                         edge-label-key
                         node-label-key))

(defun write-dot-to (file graph
                     &key
                       (edge-label-key #'identity)
                       (node-label-key #'identity))
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-dot stream graph
               :edge-label-key edge-label-key
               :node-label-key node-label-key)))

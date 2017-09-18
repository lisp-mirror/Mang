;;;; package.lisp

(defpackage #:mang
  (:use #:cl #:cl-adt #:cells #:equal #:cl-ppcre #:cl-ppcre-unicode)
  (:shadowing-import-from
   #:fset
   #:compare #:with #:less #:convert :set #:union #:@ #:filter #:image #:map
   #:empty-set #:domain #:reduce #:empty-map #:range #:size #:set-difference-2
   #:map-difference-2 #:arb #:empty? #:complement #:lookup #:intersection
   #:set-difference #:map-union #:$ #:do-map #:do-set #:map-default
   #:with-default #:last
   )
  (:shadowing-import-from
   #:cells
   #:self
   )
  (:export
   ))

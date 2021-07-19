;;;; package.lisp

(defpackage #:mang
  (:use #:cl #:cl-adt #:split-sequence)
  (:shadow #:// #:some)
  (:shadowing-import-from
   #:fset
   #:compare #:with #:less #:convert #:set #:union #:@ #:filter #:image #:map
   #:empty-set #:domain #:reduce #:empty-map #:range #:size #:set-difference-2
   #:map-difference-2 #:arb #:empty? #:complement #:lookup #:intersection
   #:set-difference #:map-union #:$ #:do-map #:do-set #:map-default
   #:with-default #:last #:collection #:remove #:find-if #:equal?
   )
  (:shadowing-import-from
   #:gmap
   #:gmap)
  (:shadowing-import-from
   #:metabang-bind
   #:bind)
  (:shadowing-import-from
   #:cl-unicode
   #:has-property)
  (:export
   ))

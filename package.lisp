;;;; package.lisp

(defpackage #:mang
  (:use
   #:cl #:equal #:cl-adt #:rope
   )
  (:shadow
   #:merge
   )
  (:shadowing-import-from
   #:fset
   #:compare #:size
   )
  (:export
   #:mult #:insert #:extract #:random-from-dist #:learn #:complete
   #:[distribution] #:<nodist>
   #:[pattern] #:<exactly> #:<this> #:<any> #:<end>
   #:[follow-db] #:<no-follow-db>
   ))

(in-package #:asdf)

(defsystem "mang-gui"
  :author "Thomas Bartscher <thomas.bartscher@weltraumschlangen.de>"
  :license "BSD3"
  :depends-on ("mang"
               "cells"
               "cells-gtk"
               )
  :components
  ((:file "gui-package")
   (:module
    "gui"
    :depends-on ("gui-package")
    :components
    ((:file "convenience")
     (:file "alphabet")
     (:file "pattern")
     (:file "learn")
     (:file "generate")
     (:file "app"
            :depends-on ("alphabet"
                         "pattern"
                         "learn"
                         "generate"
                         ))
     ))
   ))

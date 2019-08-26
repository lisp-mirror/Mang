(in-package #:mang)

(defmacro [a]if (test then &body else)
  `(bind ((it ,test))
     (if it
         ,then
         (progn
           ,@else))))

(defmacro [d]if ((predicate it &rest args)
                                 then
                 &body else)
  `(bind ((it ,it))
     (if (,predicate it ,@args)
         ,then
         (progn
           ,@else))))

(defmacro [av]if (test then &body else)
  (bind ((g!success? (gensym "success?")))
    `(bind (((:values it ,g!success?)
             ,test))
       (if ,g!success?
           ,then
           (progn
             ,@else)))))

(defmacro [a]when (test &body body)
  `([a]if ,test
       (progn
         ,@body)))

(defmacro [a]and (&rest forms)
  (if forms
      (bind (((form &rest forms)
              forms))
        (if form
            `([a]when ,form
               ([a]and ,@forms))
            form))
      't))

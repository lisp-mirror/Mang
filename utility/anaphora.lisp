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
      (chop form forms
        (if forms
            `([a]when ,form
               ([a]and ,@forms))
            form))
      't))

(defmacro [d]or (&rest forms)
  (if forms
      (chop form forms
        (if forms
            (bind ((g!test (gensym "test"))
                   ((call it &rest args)
                    form))
              `(bind ((it ,it)
                      (,g!test (,call it ,@args)))
                 (if ,g!test
                     ,g!test
                     ([d]or ,@forms))))
            form))
      nil))

(defmacro [d]setf (&rest args)
  (when args
    (chop arg args
      (bind (((&whole form _ place &rest _)
              arg))
        (if args
            `(progn
               (setf ,place ,form)
               ([d]setf ,@args))
            `(setf ,place ,form))))))

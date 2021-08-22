(in-package #:mang)

(defmacro chop (curr-var list-var &body body)
  `(bind (((,@(if (listp curr-var)
                  curr-var
                  `(,curr-var))
              &rest ,list-var)
           ,list-var))
     (declare (type list ,list-var)
              (ignorable ,list-var))
     ,@body))

(defmacro independently (&body body)
  (bind ((g!args (gensym "args")))
    `(lambda (&rest ,g!args)
       (declare (ignore ,g!args))
       ,@body)))

(defmacro mv-or (&body forms)
  (when forms
    (chop form forms
      (bind ((g!results (gensym "results"))
             (g!succeed? (gensym "succeed?")))
        `(bind (((&whole ,g!results ,g!succeed? &rest _)
                 (multiple-value-list ,form)))
           (if ,g!succeed?
               (apply #'values
                      ,g!results)
               (mv-or ,@forms)))))))

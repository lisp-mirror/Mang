(in-package #:mang)

(defadt [follow]
  (<follow> [match] [rope]))

(defmatch empty-follow? ([follow])
    boolean
  ((<follow> _ <norope>)
   t)
  ((<follow> <nomatch> _)
   t)
  (_
   nil))

(defmatch fail-follow? ([follow])
    boolean
  ((<follow> <nomatch> _)
   t)
  (_
   nil))

(defmatch match<-follow ([follow])
    [match]
  ((<follow> m _)
   m))

(defmatch rope<-follow ([follow])
    [rope]
  ((<follow> _ r)
   r))

(defmatch match-rope ([pattern] [rope] integer)
    [follow]
  ((<end> r l)
   (<follow> <m-end> (sub-rope r 0 l)))
  ((_ <norope> _)
   (<follow> <nomatch> <norope>))
  (((<any> p)
    r l)
   (let ((c (match-rope p (sub-rope r 1 (size r))
                        l)))
     (if (fail-follow? c)
         c
         (<follow> (<m-any> (match<-follow c))
                   (rope<-follow c)))))
  (((<this> p)
    r l)
   (let ((c (match-rope p (sub-rope r 1 (size r))
                        l)))
     (if (fail-follow? c)
         c
         (<follow> (<m-this> (rope-elt r 0)
                             (match<-follow c))
                   (rope<-follow c)))))
  (((<exactly> g p)
    r l)
   (if (equal? (rope-elt r 0)
               g)
       (let ((c (match-rope p (sub-rope r 1 (size r))
                            l)))
         (if (fail-follow? c)
             c
             (<follow> (<m-this> g (match<-follow c))
                       (rope<-follow c))))
       (<follow> <nomatch> <norope>))))

(defmatch chop ([pattern] [rope] integer)
    (or cons null)
  ((_ <norope> _)
   '())
  ((p r l)
   (let ((c (match-rope p r l))
         (rest (chop p (sub-rope r 1 (size r))
                     l)))
     (if (empty-follow? c)
         rest
         (cons c rest)))))

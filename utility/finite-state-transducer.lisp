(in-package #:mang)

(defclass fst ()
  ((%transitions :type map
                 :initarg :transitions
                 :reader transitions<-)
   (%preferred :type map
               :initarg :preferred
               :reader preferred<-)
   (%start-state :type symbol
                 :initarg :start-state
                 :reader start-state<-)
   (%accepting-states :type set
                      :initarg :accepting-states
                      :reader accepting-states<-)))

(defun fst (transitions &key
                          (preferred (empty-map (empty-map (empty-set))))
                          (start-state (gensym "start"))
                          (accepting-states (set (gensym "accepting"))))
  (declare (type map transitions))
  (make-instance 'fst
                 :transitions transitions
                 :preferred preferred
                 :start-state start-state
                 :accepting-states accepting-states))

(defun modify-fst (fst &key
                         transitions preferred start-state accepting-states
                         new-transitions new-preferred new-accepting-states)
  (fst (if new-transitions
           (map-union (transitions<- fst)
                      new-transitions
                      (lambda (t1 t2)
                        (map-union t1 t2
                                   #'union)))
           (or transitions (transitions<- fst)))
       :preferred
       (if new-preferred
           (map-union (preferred<- fst)
                      new-preferred
                      (lambda (t1 t2)
                        (map-union t1 t2
                                   #'union)))
           (or preferred (preferred<- fst)))
       :start-state
       (or start-state (start-state<- fst))
       :accepting-states
       (if new-accepting-states
           (union (accepting-states<- fst)
                  new-accepting-states)
           (or accepting-states (accepting-states<- fst)))))

(defun fst-elementary (condition out &key (in-state (gensym "elementary-in"))
                                       (out-state (gensym "elementary-out")))
  (fst (map (in-state (map (condition (list out out-state t))
                           :default (empty-set)))
            :default (empty-map (empty-set)))
       :start-state in-state
       :accepting-states (set out-state)))

(defun fst-sequence (fst1 fst2 &key (in-state (gensym "sequence-in"))
                                 (out-state (gensym "sequence-out")))
  (modify-fst fst1
              :start-state in-state
              :accepting-states (set out-state)
              :new-transitions
              (map ($ (transitions<- fst2))
                   ($ (map<-set (constantly (map (#'true
                                                  (list nil (start-state<- fst2)
                                                        nil))
                                                 :default (empty-set)))
                                (accepting-states<- fst1)
                                (empty-set)))
                   ($ (map<-set (constantly (map (#'true
                                                  (list nil out-state nil))
                                                 :default (empty-set)))
                                (accepting-states<- fst2)
                                (empty-set)))
                   (in-state (map (#'true (list nil (start-state<- fst1)
                                                nil))
                                  :default (empty-set)))
                   :default (empty-map (empty-set)))
              :new-preferred (preferred<- fst2)))

(defun fst-alternate (fst1 fst2 &key (in-state (gensym "alternate-in"))
                                  (out-state1 (gensym "alternate-out"))
                                  (out-state2 (gensym "alternate-out")))
  (modify-fst fst1
              :start-state in-state
              :accepting-states (set out-state1 out-state2)
              :new-transitions (todo)
              :new-preferred (preferred<- fst2)))

(defun fst-preferred (preferred fallback &key (in-state (gensym "preferred-in"))
                                           (out-state1 (gensym "preferred-out"))
                                           (out-state2 (gensym "fallback-out")))
  (modify-fst preferred
              :start-state in-state
              :accepting-states (set out-state1 out-state2)
              :new-transitions (todo)
              :new-preferred (todo)))

(defun fst-repeat (fst &key (in-state (gensym "repeat-in"))
                         (out-state (gensym "repeat-out")))
  (modify-fst fst
              :start-state in-state
              :accepting-states (set out-state)
              :new-transitions (todo)))

(defun fst-maybe (fst &key (in-state (gensym "maybe-in"))
                        (out-state1 (gensym "maybe-out"))
                        (out-state2 (gensym "maybe-not-out")))
  (modify-fst fst
              :start-state in-state
              :accepting-states (set out-state1 out-state2)
              :new-transitions (todo)))

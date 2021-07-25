(in-package #:mang)

;;;;; Definitions pertaining to nondeterministic finite state machines (NFSMs)
;;;;; and deterministic finite state machines (DFSMs). NFSMs should never be
;;;;; used anywhere but for creating DFSMs out of them. That is, outside this
;;;;; file, anything relating to NFSMs should be a method definition for the
;;;;; generic function NFSM<-.

(defun transition-closure (states transitions)
  (labels ((_rec (states acc)
             (if (empty? states)
                 acc
                 (bind ((state (arb states))
                        (next (set-difference (@ transitions state)
                                              acc)))
                   (_rec (union (less states state)
                                next)
                         (with acc state))))))
    (_rec states (empty-set))))

(defun reachable-states (states transition transitions)
  (if (empty? states)
      (empty-set)
      (bind ((state (arb states)))
        (union (@ (@ transitions state)
                  transition)
               (reachable-states (less states state)
                                 transition transitions)))))

(defun eps-with (eps-transitions source target)
  (with eps-transitions source
        (with (@ eps-transitions source)
              target)))

(defun transitions-with (transitions source transition target)
  (with transitions source
        (let ((ts (@ transitions source)))
          (with ts transition
                (with (@ ts transition)
                      target)))))

;;;; The functions for generating NFSMs all return multiple values. Defining an
;;;; own NFSM class seems like overkill (since NFSMs shouldn't be used anywhere
;;;; else either way) and returning it in a list just adds additional overhead
;;;; (I assume).
;;;; The values returned are, in this order:
;;;; START-STATE ACCEPTING-STATE TRANSITION-TABLE EPSILON-TRANSITIONS
;;;; The start state and accepting state are just objects that are only used for
;;;; their identity.
;;;; The transition table does not contain the epsilon transitions. It is a map
;;;; of states to a map of possible transitions to a set of possible states
;;;; reached by taking the former transition when being in the former state.
;;;; The epsilon transitions are a map from states to sets of states reachable
;;;; without any transition from the former state.
(defmethod nfsm<- (obj)
  (bind ((in-state (gensym "in"))
         (out-state (gensym "out")))
    (values in-state out-state
            (map (in-state (map (obj (set out-state))
                                :default (empty-set)))
                 :default (empty-map (empty-set)))
            (empty-map (empty-set)))))

(defmethod nfsm<- ((set set))
  (bind ((in (gensym "in"))
         (out (gensym "out"))
         (transitions (empty-map (empty-map (empty-set))))
         (eps-transitions (empty-map (empty-set))))
    (do-set (inner set (values in out transitions eps-transitions))
      (multiple-value-bind (inner-in inner-out inner-transitions
                                     inner-eps-transitions)
          (nfsm<- inner)
        (setf transitions
              (map-union transitions inner-transitions
                         (lambda (x y)
                           (map-union x y #'union)))
              eps-transitions
              (eps-with (eps-with (map-union eps-transitions
                                             inner-eps-transitions
                                             #'union)
                                  in inner-in)
                        inner-out out))))
    (values in out transitions eps-transitions)))

(defmethod nfsm<- ((nothing null))
  (declare (ignore nothing))
  (bind ((state (gensym "state")))
    (values state state (empty-map (empty-map (empty-set)))
            (empty-map (empty-set)))))

(defun replace-state (original replacer transition-table eps-transitions)
  (values (image (lambda (source transitions)
                   (values (if (equal? source original)
                               replacer
                               source)
                           (image (lambda (transition targets)
                                    (values transition
                                            (image (lambda (target)
                                                     (if (equal? target
                                                                 original)
                                                         replacer
                                                         target))
                                                   targets)))
                                  transitions)))
                 transition-table)
          (image (lambda (source targets)
                   (values (if (equal? source original)
                               replacer
                               source)
                           (image (lambda (target)
                                    (if (equal? target original)
                                        replacer
                                        target))
                                  targets)))
                 eps-transitions)))

(defmethod nfsm<- ((cons cons))
  (bind ((car (car cons))
         (cdr (cdr cons)))
    (if (and car cdr)
        (bind (((:values car-in car-out car-transition-table
                         car-eps-transitions)
                (nfsm<- (car cons)))
               ((:values cdr-in cdr-out cdr-transition-table
                         cdr-eps-transitions)
                (nfsm<- (cdr cons)))
               ((:values cdr-transition-table cdr-eps-transitions)
                (replace-state cdr-in car-out cdr-transition-table
                               cdr-eps-transitions)))
          (values car-in cdr-out (map-union car-transition-table
                                            cdr-transition-table
                                            (lambda (x y)
                                              (map-union x y #'union)))
                  (map-union car-eps-transitions
                             cdr-eps-transitions
                             #'union)))
        (nfsm<- (or car cdr)))))

(defun dfsm-simplify-state-names (dfsm start-state accepting-states)
  (bind ((failure (gensym "failure"))
         (start (gensym "start"))
         (accepting (convert 'map
                             (image (lambda (accepting-state)
                                      (cons accepting-state
                                            (gensym "accept")))
                                    accepting-states)))
         (registry (map (start-state start)
                        ((empty-set)
                         failure)
                        ($ accepting)))
         (new-dfsm (empty-map (empty-map failure))))
    (do-map (in transitions dfsm)
      (bind ((in-simple (or (@ registry in)
                            (gensym "state"))))
        (setf registry (with registry in in-simple))
        (do-map (category target transitions)
          (bind ((target-simple (or (@ registry target)
                                    (gensym "state"))))
            (setf registry (with registry target target-simple))
            (setf new-dfsm
                  (with new-dfsm in-simple
                        (with (@ new-dfsm in-simple)
                              category target-simple)))))))
    (values new-dfsm start (range accepting)
            failure)))

(defun dfsm<-nfsm (start-state transition-map epsilon-transitions
                   accepting-state)
  (bind ((inputs (expand (lambda (state transitions)
                           (declare (ignore state))
                           (domain transitions))
                         transition-map)))
    (labels
        ((_rec (todo done acc)
           (if (empty? todo)
               acc
               (bind ((current (arb todo)))
                 (if (@ current done)
                     (_rec (less todo current)
                           done acc)
                     (let ((new (convert 'map
                                         (image (lambda (input)
                                                  (cons input
                                                        (transition-closure
                                                         (reachable-states
                                                          current input
                                                          transition-map)
                                                         epsilon-transitions)))
                                                inputs))))
                       (_rec (less (union todo (set-difference (range new)
                                                               done))
                                   current)
                             (with done current)
                             (with acc current new))))))))
      (bind ((start-state (transition-closure (set start-state)
                                              epsilon-transitions))
             (dfsm (_rec (set start-state)
                         (empty-set)
                         (empty-map (empty-map (empty-set))))))
        (dfsm-simplify-state-names dfsm start-state (keep (lambda (state)
                                                            (@ state
                                                               accepting-state))
                                                          (domain dfsm)))))))

(defun collapse-states (transition-table)
  (bind ((valuable-states (empty-map))
         (redirections (empty-map))
         (new-transition-table (empty-map (empty-map (empty-set)))))
    (do-map (state transitions transition-table)
      (unless (@ valuable-states transitions)
        (setf valuable-states
              (with valuable-states transitions state)))
      (bind ((new-state (@ valuable-states transitions)))
        (setf redirections
              (with redirections state new-state))
        (setf new-transition-table
              (with new-transition-table new-state transitions))))
    (image (lambda (state transitions)
             (values state (image (lambda (transition target)
                                    (values transition (@ redirections target)))
                                  transitions)))
           new-transition-table)))

(defun collapse-states* (transition-table)
  (bind ((collapsed (collapse-states transition-table)))
    (if (equal? transition-table collapsed)
        collapsed
        (collapse-states* collapsed))))

(defun prune-failure-dfsm (transitions failure)
  (image (lambda (state transitions)
           (values state
                   (filter (lambda (transition target)
                             (declare (ignore transition))
                             (not (equal? target failure)))
                           transitions)))
         (filter (lambda (state transitions)
                   (declare (ignore transitions))
                   (not (equal? state failure)))
                 transitions)))

(defclass dfsm ()
  ((%transitions :type map
                 :initarg :transitions
                 :initform (empty-map (empty-map (empty-set)))
                 :reader transitions<-)
   (%start-state :initarg :start-state
                 :reader start-state<-)
   (%accepting-states :type set
                      :initarg :accepting-states
                      :initform (empty-set)
                      :reader accepting-states<-)))

(defmethod dfsm<- (obj)
  (bind (((:values in out transitions eps-transitions)
          (nfsm<- obj))
         ((:values transitions in outs failure)
          (dfsm<-nfsm in transitions eps-transitions out)))
    (make-instance 'dfsm
                   :transitions
                   (collapse-states* (prune-failure-dfsm transitions failure))
                   :start-state in
                   :accepting-states outs)))

(defmethod generate ((generator dfsm)
                     (markov map)
                     &optional negative)
  (declare (type (or map null)
                 negative))
  (bind ((transition-map (transitions<- generator))
         (accepting (accepting-states<- generator)))
    (labels ((_rec (state acc)
               (bind ((transitions (domain (@ transition-map state))))
                 (if (or (empty? transitions)
                         ;; the following part of the check should be changed –
                         ;; as it is now, the generator simply prefers shorter
                         ;; words
                         (and (@ accepting state)
                              (= (random 6)
                                 0)))
                     acc
                     (bind ((dist (keep transitions
                                        (if negative
                                            (diminish
                                             (filtering-combine acc markov
                                                                #'union #'mult
                                                                <nodist>)
                                             (filtering-combine acc negative
                                                                #'union #'mult
                                                                <nodist>))
                                            (filtering-combine acc markov
                                                               #'union #'mult
                                                               <nodist>))))
                            (transition (extract-random dist)))
                       (_rec (@ (@ transition-map state)
                                transition)
                             (nconc acc (list transition))))))))
      (_rec (start-state<- generator)
            '()))))

;;;; The following implementations for the matching via deterministic finite
;;;; state machine assume that there is a maximum length that the given DFSM can
;;;; match. This forbids matching for anything containing the * modificator, but
;;;; for matching reasonable words this is a perfectly reasonable
;;;; expectation. The DFSM builders implemented here definitely should only
;;;; produce DFSMs matching only constructs under a given length. If not, that
;;;; should be considered a bug.
(defmethod run-dfsm ((dfsm dfsm)
                     (word cons))
  (bind ((transition-table (transitions<- dfsm)))
    (labels ((_rec (state word)
               (if word
                   (_rec (@ (@ transition-table state)
                            (first word))
                         (rest word))
                   (@ (accepting-states<- dfsm)
                      state))))
      (_rec (start-state<- dfsm)
            word))))

(defmethod run-dfsm ((dfsm dfsm)
                     (word string))
  (bind ((transition-table (transitions<- dfsm))
         (accepting-states (accepting-states<- dfsm)))
    (labels ((_rec (state word)
               (bind ((transitions (@ transition-table state))
                      (valid-transitions (filter (lambda (transition)
                                                   (bind
                                                       ((pos (search transition
                                                                     word)))
                                                     (and pos (= pos 0))))
                                                 (domain transitions))))
                 (cond
                   ((and (string= word "")
                         (@ accepting-states state))
                    (set '()))
                   ((empty? valid-transitions)
                    (empty-set))
                   (t
                    (bind ((results (empty-set)))
                      (do-set (transition valid-transitions results)
                        (bind ((found
                               (_rec (@ transitions transition)
                                     (subseq word (length transition)))))
                          (setf results
                                (union results (image (lambda (found)
                                                        (cons transition
                                                              found))
                                                      found)))))))))))
      (_rec (start-state<- dfsm)
            word))))

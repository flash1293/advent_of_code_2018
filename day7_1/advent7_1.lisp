#|
Well, I chose not to perceive the given desciption as a directed graph, but 
to see the input as a set of rules. So the solution is based on a simple 
agorithm  out of the area of reasoning, called hypothesize-and-test.
To use clos/some classes would definitely improve readability ;-)
|#



(defparameter testinput "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

;  -->A--->B--
; /    \      \
;C      -->D----->E
; \           /
;  ---->F-----


(defparameter realinput "Step I must be finished before step G can begin.
Step J must be finished before step A can begin.
Step L must be finished before step D can begin.
Step V must be finished before step S can begin.
Step U must be finished before step T can begin.
Step F must be finished before step Z can begin.
Step D must be finished before step A can begin.
Step E must be finished before step Z can begin.
Step C must be finished before step Q can begin.
Step H must be finished before step X can begin.
Step A must be finished before step Z can begin.
Step Z must be finished before step M can begin.
Step P must be finished before step Y can begin.
Step N must be finished before step K can begin.
Step R must be finished before step W can begin.
Step K must be finished before step O can begin.
Step W must be finished before step S can begin.
Step G must be finished before step Q can begin.
Step Q must be finished before step B can begin.
Step S must be finished before step T can begin.
Step B must be finished before step M can begin.
Step T must be finished before step Y can begin.
Step M must be finished before step O can begin.
Step X must be finished before step O can begin.
Step O must be finished before step Y can begin.
Step C must be finished before step O can begin.
Step B must be finished before step O can begin.
Step T must be finished before step O can begin.
Step S must be finished before step X can begin.
Step E must be finished before step K can begin.
Step Q must be finished before step M can begin.
Step E must be finished before step P can begin.
Step Q must be finished before step S can begin.
Step E must be finished before step O can begin.
Step D must be finished before step P can begin.
Step X must be finished before step Y can begin.
Step I must be finished before step U can begin.
Step B must be finished before step X can begin.
Step F must be finished before step T can begin.
Step B must be finished before step T can begin.
Step V must be finished before step R can begin.
Step I must be finished before step Q can begin.
Step I must be finished before step A can begin.
Step M must be finished before step X can begin.
Step Z must be finished before step S can begin.
Step C must be finished before step S can begin.
Step T must be finished before step M can begin.
Step K must be finished before step X can begin.
Step Z must be finished before step P can begin.
Step V must be finished before step H can begin.
Step Z must be finished before step B can begin.
Step M must be finished before step Y can begin.
Step C must be finished before step K can begin.
Step W must be finished before step Y can begin.
Step J must be finished before step Z can begin.
Step Q must be finished before step O can begin.
Step T must be finished before step X can begin.
Step P must be finished before step Q can begin.
Step P must be finished before step K can begin.
Step D must be finished before step M can begin.
Step P must be finished before step N can begin.
Step S must be finished before step B can begin.
Step H must be finished before step Y can begin.
Step R must be finished before step K can begin.
Step G must be finished before step S can begin.
Step P must be finished before step S can begin.
Step C must be finished before step Z can begin.
Step Q must be finished before step Y can begin.
Step F must be finished before step R can begin.
Step N must be finished before step B can begin.
Step G must be finished before step M can begin.
Step E must be finished before step X can begin.
Step D must be finished before step E can begin.
Step D must be finished before step C can begin.
Step U must be finished before step O can begin.
Step H must be finished before step Z can begin.
Step L must be finished before step C can begin.
Step L must be finished before step F can begin.
Step V must be finished before step D can begin.
Step F must be finished before step X can begin.
Step V must be finished before step W can begin.
Step S must be finished before step Y can begin.
Step K must be finished before step T can begin.
Step D must be finished before step Z can begin.
Step C must be finished before step W can begin.
Step V must be finished before step M can begin.
Step F must be finished before step H can begin.
Step A must be finished before step M can begin.
Step G must be finished before step Y can begin.
Step H must be finished before step M can begin.
Step N must be finished before step W can begin.
Step J must be finished before step K can begin.
Step C must be finished before step B can begin.
Step Z must be finished before step Y can begin.
Step L must be finished before step E can begin.
Step G must be finished before step B can begin.
Step Q must be finished before step T can begin.
Step D must be finished before step W can begin.
Step H must be finished before step G can begin.
Step L must be finished before step O can begin.
Step N must be finished before step O can begin.")



; parseline is more complicated than needed to be able to read nodenames longer than one char
(defun parseline (line)
  (if (string= "" line)
      nil
      (let* ((key1 "Step")
             (key2 "step")
             (seperator " ")
             (start1 (+ (search key1 line) (length key1) (length seperator))) 
             (end1 (search  " " line  :start2 start1))
             (precondition (subseq line start1 end1))
             (start2 (+ (search key2 line) (length key2) (length seperator))) 
             (end2 (search  " " line  :start2 start2))
             (postcondition (subseq line start2 end2)))
        (list precondition postcondition)
        )))

; (parseline  "Step C1 must be finished before step 1-F'3 can begin.")


; returns a list (list-of-startnodes hastable-of-forwardrules hashtable-of-preconditions)
(defun parse (inputstrings) 
  (let ((instream (make-string-input-stream inputstrings))
        (rules (make-hash-table  :test 'equal))
        (backwardchaining (make-hash-table  :test 'equal)) 
        (facts '())
        (hypothesis '()))
    (do*  ((l (parseline (read-line instream nil ""))  (parseline (read-line instream nil "")))
           (precondition (first l) (first l))
           (postcondition (second l) (second l)))
          ((not (consp l)))
      (setf (gethash precondition rules) (cons postcondition (gethash precondition rules)))
      (setf (gethash postcondition backwardchaining) (cons precondition (gethash postcondition backwardchaining)))
      (setq facts (adjoin precondition facts :test #'string=))
      (setq hypothesis (adjoin postcondition hypothesis :test #'string=)))
        (dolist (postcondition hypothesis)
          (setq facts (remove postcondition facts :test #'string=)))
    (list facts rules backwardchaining)
    ))

; (parse inp)

;inspect the hashtables
; (maphash #'(lambda (k v) (print (list k v))) (second (parse testinput)))
; (maphash #'(lambda (k v) (print (list k v))) (third (parse testinput)))
; (maphash #'(lambda (k v) (print (list k v))) (second (parse realinput)))
; (maphash #'(lambda (k v) (print (list k v))) (third (parse realinput)))


; checks if all conditions of  rule are true, so that the rule may fire.
(defun checkpreconditions (facts preconditions)
  (let ((result T))
    (dolist (p preconditions)
      (unless (member p facts :test #'string=)
        (setq result nil)))
    result))

; (checkpreconditions '("C") (gethash "A" (third (parse inp))))
; (checkpreconditions '("C") (gethash "E" (third (parse inp))))



(defun hypothesizse-and-test (startnodes rules preconditions)
  (let ((facts'())
        (hypotheses  (sort startnodes #'string<)))
    (do* ((i 0 (1+ i))
          (currenthypothesis (nth i (sort hypotheses #'string<)) (nth i (sort hypotheses #'string<)))
          (currentrules (gethash currenthypothesis rules)  (gethash currenthypothesis rules)))
         ((>= i (length hypotheses)))
      (when (checkpreconditions facts (gethash currenthypothesis preconditions))
        (setq hypotheses (append (subseq hypotheses 0 i) (subseq hypotheses (+ 1 i))))
        (setq i -1)
        (setq hypotheses (sort (union currentrules hypotheses :test #'string=) #'string<))
        (setq facts (cons currenthypothesis facts))
        ))    
    (format NIL "~{~a~}" (reverse facts))
    ))


; (apply #'hypothesizse-and-test (parse testinput)) --> "CABDFE"
; (apply #'hypothesizse-and-test (parse realinput)) --> "IJLFUVDACEHGRZPNKQWSBTMXOY"

(apply #'hypothesizse-and-test (parse realinput))


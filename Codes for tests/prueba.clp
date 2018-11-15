

;;****************
;;* DEFFUNCTIONS *
;;****************



(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

;; Pregunta tipo "Quiere otra chica?"
(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))



;;;***************
;;;* QUERY RULES *
;;;***************







;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (printout t crlf crlf)
  (printout t "Welcome to studio assistant")
  (printout t crlf crlf))


(defrule print-bolita ""
  (declare (salience 10))
  (bolita ?item)
  =>
  (printout t crlf crlf)
  (printout t "Bolita sacada:")
  (printout t crlf crlf)
  (format t " %s%n%n%n" ?item))


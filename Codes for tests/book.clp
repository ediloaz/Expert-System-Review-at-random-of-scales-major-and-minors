(deftemplate book
    (multislot surname)(slot name)(multislot title) 
)

(deffacts initial
(book (surname J.P.)(name Dubreuil)(title History of francmasons))
(book (surname T.)(name Eker)(title Secrets of millionaire mind)))

(defrule find_title
    ?book<-(book(name Eker))
    =>
    (printout t ?book crlf)
)
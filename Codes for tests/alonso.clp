(deftemplate Alonso-Cursos
	(slot nombres)
)

(deffacts initial
(Alonso-Cursos (nombres Inteligencia-Artificial))
(Alonso-Cursos (nombres Redes)))

(defrule buscar_curso
	?Alonso-Cursos<-(Alonso-Cursos(nombres Redes))
	=>
	(printout t ?Alonso-Cursos crlf)
)

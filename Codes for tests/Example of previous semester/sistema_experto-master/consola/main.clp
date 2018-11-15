
;;===================== Acerca de ==========================
;;TECNOLOGICO DE COSTA RICA
;;Inteligencia Artificial
;;Sistema experto
;;Carlos Fernández Jiménez
;;Kenneth Quiros
;;===================== Comandos ==========================
;;(clear)
;;(load ./main.clp)
;;(reset)
;;(run)
;;===================== Template ==========================
(deftemplate intervalo
        (slot  semitono)
        (slot  nombre)
        (slot  simbolo)
)

(deftemplate notacion
        (slot  indice)
        (slot  nombre)
        (slot  altura)
)

(deftemplate nota
        (slot nombre)
        (slot  altura)
        (slot sistemaTonal)
)

(deftemplate triada
        (multislot acorde_1)
        (multislot acorde_2)
        (multislot acorde_3)
)

(deftemplate notasDisponibles
        (multislot notas)
)

(deftemplate notasDisponibleOrdenadas
        (multislot notas)
)

(deftemplate notasPorPrimerNota
        (multislot notas)
)
;;estado inicial del programa
;;se define un estado con valor 0 por default para
;;indicar que el acorde no es valido, en caso de ser 1 significa que si es valido
(deffacts estadoInicial
  (esValido 0)
  (gradoNota I Tónica 1)
  (gradoNota ii Supertónica 2)
  (gradoNota iii Mediante 3)
  (gradoNota IV Subdominante 4)
  (gradoNota V Dominante 5)
  (gradoNota vi Superdominante 6)
  (gradoNota vi^i Sensible 7)
)

;;===================== Regla Inicio ==========================
(defrule main

        =>
        (assert (inicio))
)
;;obtiene el tipo de notacion a usar en el sistema
(defrule obtenerNotacion
        ?indice <- (inicio)
        =>
        ;;(load-facts "intervalo.dat")
        (printout t crlf "Sistema Experto" crlf crlf
                         "Indique el sistema de notacion que prefiere:" crlf
                         "-Digite 1 para Inglesa" crlf
                         "-Digite 2 para Italiana" crlf
                         "-Mi opcion elegida es: "
        )
        (bind ?tipoNotacion (read))
        (assert(notacionSeleccionada ?tipoNotacion))
        (retract ?indice)
)
;;cargar notacion seleccionada por el usuario si es valida
;;1 para italiana, 2 para inglesa
;;obtiene el tipo de tonalidad
(defrule esOpcionNotacionValida
        (notacionSeleccionada ?notacion)
        (or (test (eq ?notacion 1))
            (test (eq ?notacion 2)))
        =>
        (load-facts (str-cat "notacion_" ?notacion ".dat"))
        (printout t crlf "Indique como indicara la tonalidad:" crlf
                         "-Digite 1 para nombre" crlf
                         "-Digite 2 para numero y tipo de alteraciones" crlf
                         "Mi opcion elegida es: "
        )
        (bind ?tipoTonalidad (read))
        (assert(tonalidadSeleccionada ?tipoTonalidad))
)
;;valida si el tipo de notacion seleccionada es no valida
;;envia a estado de error si se cumple
(defrule esOpcionNotacionInvalida
        ?indice <- (notacionSeleccionada ?notacion)
        (and (not (test (eq ?notacion 1)))
             (not (test (eq ?notacion 2))))
        =>
        (retract ?indice)
        (printout t"Error: La notacion no es valida" crlf crlf)
        (assert (inicio))
)
;;valida si el tipo de tonalidad seleccionada es no valida
;;envia a estado de error si se cumple
(defrule esOpcionTonalidadInvalida
        ?indice <- (tonalidadSeleccionada  ?tonalidad)
        ?indiceNotacion <- (notacionSeleccionada ?notacion)
        (and (not (test (eq ?tonalidad 1)))
             (not (test (eq ?tonalidad 2))))
        =>
        (retract ?indiceNotacion ?indice)
        (printout t "Error: La opcion para tonalidad no es valida: " tonalidad crlf crlf)
        (assert (notacionSeleccionada  ?notacion))
)
;;obtiene el nombre de la tonalidad ingresada
(defrule esTonalidadPorNombre
        (tonalidadSeleccionada 1)
        =>
        (printout t crlf "-Digite el nombre de la tonalidad:")
        (bind ?nombreTonalidad (read))
        (assert (tonalidad ?nombreTonalidad))
)
;;obtiene el tipo de alteracion y el numero de ellas,
;;acorde a una tonalidad
(defrule esTonalidadPorAlteraciones
        (tonalidadSeleccionada 2)
        =>
        (printout t crlf "-Digite el numero de alteraciones, rango [0,7]:" crlf)
        (bind ?numeroAlteraciones (read))
        (printout t "-Digite el tipo de alteracion (b ó #):" crlf)
        (bind ?tipoAlteracion (read))
        (assert (tonalidad ?numeroAlteraciones ?tipoAlteracion))
)
;;validad si el nombre de la tonalidad ingresada es valida
;;(defrule esTonalidadNombreValido
  ;;      (tonalidad ?tonalidadIngresada)
    ;;    ?indiceTonalidad <- (tonalidadSeleccionada  ?tonalidad)
      ;;  (notacion (indice ?) (nombre ?tonalidadIngresada) (altura ?))
        ;;;=>
      ;;;  (retract ?indiceTonalidad)
;;)
;;si el nombre de la tonalidad no es validad imprime el error
(defrule esTonalidadNombreInvalido
        ?indice <- (tonalidad ?tonalidadIngresada)
        ?indiceTonalidad <- (tonalidadSeleccionada  ?tonalidad)
        (not (notacion (indice ?) (nombre ?tonalidadIngresada) (altura ?)))
        =>
        (retract ?indiceTonalidad ?indice)
        (printout t"Error: La tonalidad ingresada no es valida: " crlf crlf)
        (assert (tonalidadSeleccionada  ?tonalidad))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;verifica que el numero de altaraciones según un tipo de alteracion,
;;corresponda a una tonalidad de sostenidos
(defrule esTonalidadAlteracionSostenidosValido
        ?indice <- (tonalidad ?numero ?alteracion)
        ?indiceTonalidad <- (tonalidadSeleccionada  ?tipoTonalidad)
        (test (> ?numero -1))
        (test (< ?numero 8))
        (test (eq ?alteracion #))
        (notacion (indice ?numero) (nombre ?tonalidad) (altura ?))
        =>
        (retract ?indice ?indiceTonalidad )
        (assert (tonalidad ?tonalidad))
)
;;convierte el numero de alteraciones a un valor negativo para
;;poder verificar si es valido
(defrule esTonalidadAlteracionBemolesValido
        ?indice <- (tonalidad ?numero ?alteracion)
        ?indiceTonalidad <- (tonalidadSeleccionada  ?tipoTonalidad)
        (test (> ?numero -1))
        (test (< ?numero 8))
        (test (eq ?alteracion b))
        =>
        (bind ?resultado (- 0 ?numero))
        (retract ?indice ?indiceTonalidad )
        (assert (tonalidad ?resultado ?alteracion))
)
;;verifica que el numero de altaraciones según un tipo de alteracion,
;;corresponda a una tonalidad de bemoles
(defrule esTonalidadAlteracionBemolesNegativoValido
        ?indice <- (tonalidad ?numero ?alteracion)
        (test (< ?numero 1))
        (test (> ?numero -8))
        (test (eq ?alteracion b))
        (notacion (indice ?numero) (nombre ?tonalidad) (altura ?))
        =>
        (retract ?indice)
        (assert (tonalidad ?tonalidad))

)
;;Validación de un Rango falta
(defrule esTonalidadAlteracionInvalido
        ?indice <- (tonalidad ?numero ?alteracion)
        ?indiceTonalidad <- (tonalidadSeleccionada  ?tonalidad)
        (or (not (and (test (> ?numero -1))
                      (test (< ?numero 8)))
            )
            (not (or (test (eq ?alteracion b))
                     (test (eq ?alteracion #))
                  )
            )
        )
        =>
        (retract ?indiceTonalidad ?indice)
        (printout t"Error: El numero ó el tipo de alteración no es valida" crlf crlf)
        (assert (tonalidadSeleccionada  ?tonalidad))
)
;;carga la escala de notas de la tonalidad en sostenidos
(defrule usarNotasSostenidos
        ?indiceTipoNotacion <- (notacionSeleccionada ?notacion)
        (tonalidad ?tonalidad)
        (notacion (indice ?indice) (nombre ?tonalidad) (altura ?))
        (test (> ?indice -1))
        =>
        (load-facts (str-cat "sostenidos_" ?notacion ".dat"))
        (retract ?indiceTipoNotacion)
)
;;carga la escala de notas de la tonalidad en bemoles
(defrule usarNotasBemoles
        ?indiceTipoNotacion <- (notacionSeleccionada ?notacion)
        (tonalidad ?tonalidad)
        (notacion (indice ?indice) (nombre ?tonalidad) (altura ?))
        (test (< ?indice 0))
        =>
        (load-facts (str-cat "bemoles_" ?notacion ".dat"))
        (retract ?indiceTipoNotacion)
)
;;ordena las notas para que inicien por la tonalidad dada
(defrule ordenarNotasDisponibles
        (tonalidad ?tonalidad)
        ?indice <- (notasDisponibles(notas $?inicio ?tonalidad $?fin))
        =>
        (assert (notasDisponibleOrdenadas (notas ?tonalidad $?fin $?inicio)))
        ;;(retract ?indice)
)
;;genera la escala mayor segun la tonalidad
(defrule generarEscalaMayor
        (notasDisponibleOrdenadas (notas ?n1 ?n2 ?n3 ?n4 ?n5 ?n6 ?n7 ?n8 ?n9 ?n10 ?n11 ?n12))
        =>
        (assert (escalaMayor ?n1 ?n3 ?n5 ?n6 ?n8 ?n10 ?n12))
        (assert (notasInvalidas))
        (printout t crlf "Escala: " ?n1 " - " ?n3 " - " ?n5 " - " ?n6 " - " ?n8 " - " ?n10 " - " ?n12 crlf)
        (assert (acorde))

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;obtiene los datos para formar la triada
(defrule obtenerAcorde
        ?indice <- (acorde) ;;revisar si se cambia
        (notasDisponibles (notas $?listaNotas))
        =>
        (printout t crlf "Ahora formemos el acorde!" crlf)
        (printout t crlf "-Digite la primer nota: ");;;validar la forma de ingresar datos
        (bind ?primerNota (read))
        (printout t "-Digite la primera altura: ");;;validar la forma de ingresar datos
        (bind ?primerAltura (read))
        (printout t "-Digite la segunda nota: ");;;validar la forma de ingresar datos
        (bind ?segundaNota (read))
        (printout t "-Digite la segunda altura: ");;;validar la forma de ingresar datos
        (bind ?segundaAltura (read))
        (printout t "-Digite la tercer nota: ");;;validar la forma de ingresar datos
        (bind ?tercerNota (read))
        (printout t "-Digite la tercer altura: ");;;validar la forma de ingresar datos
        (bind ?tercerAltura (read))
        (retract ?indice)
        (assert (notas ?primerNota  ?segundaNota ?tercerNota));;es para luego ordenarlas
        (bind ?resultado_1 (+ (* ?primerAltura 12) (member$ ?primerNota $?listaNotas)))
        (bind ?resultado_2 (+ (* ?segundaAltura 12) (member$ ?segundaNota $?listaNotas)))
        (bind ?resultado_3 (+ (* ?tercerAltura 12) (member$ ?tercerNota $?listaNotas)))
        (assert (alturas ?resultado_1 ?resultado_2 ?resultado_3))
        (assert (acorde ?primerNota ?primerAltura ?resultado_1)
                (acorde ?segundaNota ?segundaAltura ?resultado_2)
                (acorde ?tercerNota ?tercerAltura ?resultado_3)
        )
)
;;verifica que las notas de los acordes pertenezcan a la escala de la tonalidad
;;si todas las notas pertenecen a la escala la lista de notas queda vacia
(defrule notasEnEscalaValido
        ?indice <- (notas ?cabeza $?cola)
        (escalaMayor $?inicio ?cabeza $?fin)
        =>
        (retract ?indice)
        (assert (notas $?cola))
)
;;verifica que las notas de los acordes no pertenezcan a la escala de la tonalidad
;;agrega las notas que no pertenezcan a una lista de notas no validas
(defrule notasEnEscalaInvalido
        ?indice <- (notas ?cabeza $?cola)
        ?indiceErrores <- (notasInvalidas $?notasInvalidas)
        (escalaMayor $?escala)
        (test (> (length$ $?escala) 0))
        (not (escalaMayor $?inicio ?cabeza $?fin))
        =>
        (retract ?indice ?indiceErrores)
        (assert (notasInvalidas $?notasInvalidas ?cabeza))
        (assert (notas $?cola))
)
;;imprime en pantalla las notas que no pertenecen a la escala
;;de la tonalidad dada, se redirige a un menu de opciones
(defrule mostrarNotasEnEscalaInvalido
        (notasInvalidas $?notas)
        (test (> (length$ $?notas) 0))
        (tonalidad ?tonalidad)
        =>
        (printout t crlf "Error: Las siguientes notas no son válidas en la escala de " ?tonalidad ":" crlf )
        (printout t  $?notas crlf crlf)
)
;;ordena las alturas de los acordes de menor a mayor
(defrule ordenarAlturas
        ?indiceAlturas <- (alturas $?inicio ?num1 ?num2 $?fin)
        (test (> ?num1 ?num2))
        =>
        (assert (alturas $?inicio ?num2 ?num1 $?fin))
        (retract ?indiceAlturas)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;crea una triada con 3 acordes, ordenada por su altura
;;cuando sus los 3 acordes tienen altura distinta
(defrule ordenarTriadaAlturas;;hacer otra para que agarre la vara y de error
        (notas $?notas)
        (test (= (length$ $?notas) 0))
        ?indiceAltura <- (alturas ?resultado_1 ?resultado_2 ?resultado_3)
        (and (test (< ?resultado_1 ?resultado_2))
             (test (< ?resultado_2 ?resultado_3))
        )
        ?indiceAcorde1 <- (acorde ?primerNota ?primerAltura ?resultado_1)
        ?indiceAcorde2 <- (acorde ?segundaNota ?segundaAltura ?resultado_2)
        ?indiceAcorde3 <- (acorde ?tercerNota ?tercerAltura ?resultado_3)
        =>
        (assert (triada (acorde_1 ?primerNota ?primerAltura)
                        (acorde_2 ?segundaNota ?segundaAltura)
                        (acorde_3 ?tercerNota ?tercerAltura)
                )
        )
        (retract ?indiceAltura ?indiceAcorde1 ?indiceAcorde2 ?indiceAcorde3)
)
;;acomodar la escala con respecto a la tonica
(defrule ordenarEscala
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        ?indice <- (notasDisponibleOrdenadas (notas $?inicio ?primerNota $?fin))
        =>
        ;;(retract ?indice)
        (assert (notasPorPrimerNota (notas ?primerNota $?fin $?inicio)))
)
;;verifica las distancias entre los acordes para saber si la triada
;;es una escala mayor con respecto a la tonalidad dada
(defrule esAcordeMayorValido
        ?indice <- (esValido 0)
        (notasPorPrimerNota (notas $?notas))
        (test (> (length$ $?notas) 0))
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        (test (= (- (member$ ?primerNota $?notas) 1) 0))
        (test (= (- (member$ ?segundaNota $?notas) 1) 4))
        (test (= (- (member$ ?tercerNota $?notas) 1) 7))
        =>
        (retract ?indice)
        (assert (esValido 1))
        (printout t crlf "Es acorde mayor "crlf crlf)
)
;;verifica las distancias entre los acordes para saber si la triada
;;es una escala menor con respecto a la tonalidad dada
(defrule esAcordeMenorValido
        ?indice <- (esValido 0)
        (notasPorPrimerNota (notas $?notas))
        (test (> (length$ $?notas) 0))
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        (test (= (- (member$ ?primerNota $?notas) 1) 0))
        (test (= (- (member$ ?segundaNota $?notas) 1) 3))
        (test (= (- (member$ ?tercerNota $?notas) 1) 7))
        =>
        (retract ?indice)
        (assert (esValido 1))
        (printout t crlf "Es acorde menor "crlf crlf)
)
;;verifica las distancias entre los acordes para saber si la triada
;;es una escala menor con respecto a la tonalidad dada
(defrule esAcordeDisminuidoValido
        ?indice <- (esValido 0)
        (notasPorPrimerNota (notas $?notas))
        (test (> (length$ $?notas) 0))
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        (test (= (- (member$ ?primerNota $?notas) 1) 0))
        (test (= (- (member$ ?segundaNota $?notas) 1) 3))
        (test (= (- (member$ ?tercerNota $?notas) 1) 6))
        =>
        (retract ?indice)
        (assert (esValido 1))
        (printout t crlf "Es acorde disminuido "crlf crlf)
)
;;verifica las distancias entre los acordes para saber si la triada
;;es la primer inversion de la escala mayor con respecto a la tonalidad dada
(defrule esAcordePrimerInversionValido
        ?indice <- (esValido 0)
        (tonalidad ?tonalidad)
        (notasPorPrimerNota (notas $?notasDisponibles))
        (test (> (length$ $?notasDisponibles) 0))
        (notasPorPrimerNota (notas $?notas))
        (test (> (length$ $?notas) 0))
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        (test (= (- (member$ ?tonalidad $?notasDisponibles) 1) 8))
        (test (= (- (member$ ?primerNota $?notas) 1) 0))
        (test (= (- (member$ ?segundaNota $?notas) 1) 3))
        (test (= (- (member$ ?tercerNota $?notas) 1) 8))
        =>
        (retract ?indice)
        (assert (esValido 2))
        (printout t crlf "Es la primera inversion del acorde mayor "crlf crlf)
)
;;verifica las distancias entre los acordes para saber si la triada
;;es la segunda inversion de la escala mayor con respecto a la tonalidad dada
(defrule esAcordeSegundaInversionValido
        ?indice <- (esValido 0)
        (tonalidad ?tonalidad)
        (notasPorPrimerNota (notas $?notasDisponibles))
        (test (> (length$ $?notasDisponibles) 0))
        (notasPorPrimerNota (notas $?notas))
        (test (> (length$ $?notas) 0))
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        (test (= (- (member$ ?tonalidad $?notasDisponibles) 1) 5))
        (test (= (- (member$ ?primerNota $?notas) 1) 0))
        (test (= (- (member$ ?segundaNota $?notas) 1) 5))
        (test (= (- (member$ ?tercerNota $?notas) 1) 9))
        =>
        (retract ?indice)
        (assert (esValido 2))
        (printout t crlf "Es la segunda inversion del acorde mayor "crlf crlf)
)
;;determina el grado de la nota en la escala, solo si el acorde es valido (esValido 1)
;;se debe calcular la distancia que existe entre la tonalidad
;;y la tonica
(defrule calcularGradoNota
        (esValido 1)
        (escalaMayor $?escala)
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        =>
        (bind ?valor (member$ ?primerNota $?escala))
        (assert  (valorGrado ?valor))

)
;;imprime el grado de la nota y su nombre
(defrule imprimirGradoNota
        (valorGrado ?valor)
        (gradoNota ?simbolo ?nombre ?valor)
        =>
        (printout t ?simbolo " - " ?nombre crlf crlf)

)
;;captura si el acorde no es valido
(defrule noEsAcorde
        (esValido 0)
        (tonalidad ?tonalidad)
        (notasPorPrimerNota (notas $?notasDisponibles))
        (test (> (length$ $?notasDisponibles) 0))
        (notasPorPrimerNota (notas $?notas))
        (test (> (length$ $?notas) 0))
        (triada (acorde_1 ?primerNota ?primerAltura)
                (acorde_2 ?segundaNota ?segundaAltura)
                (acorde_3 ?tercerNota ?tercerAltura)
        )
        (or (or (not (test (= (- (member$ ?primerNota $?notas) 1) 0)))
                (not (test (= (- (member$ ?segundaNota $?notas) 1) 4)))
                (not (test (= (- (member$ ?tercerNota $?notas) 1) 7)))
            )
            (or (not (test (= (- (member$ ?primerNota $?notas) 1) 0)))
                (not (test (= (- (member$ ?segundaNota $?notas) 1) 3)))
                (not (test (= (- (member$ ?tercerNota $?notas) 1) 7)))
            )
            (or (not (test (= (- (member$ ?tonalidad $?notasDisponibles) 1) 8)))
                (not (test (= (- (member$ ?primerNota $?notas) 1) 0)))
                (not (test (= (- (member$ ?segundaNota $?notas) 1) 3)))
                (not (test (= (- (member$ ?tercerNota $?notas) 1) 8)))
            )
            (or (not (test (= (- (member$ ?tonalidad $?notasDisponibles) 1) 5)))
                (not (test (= (- (member$ ?primerNota $?notas) 1) 0)))
                (not (test (= (- (member$ ?segundaNota $?notas) 1) 5)))
                (not (test (= (- (member$ ?tercerNota $?notas) 1) 9)))
            )
        )
        =>
        (printout t crlf "El acorde  no es válido con respecto a la tonalidad" crlf crlf)
)








;;link de funciones clips
;;https://www.csie.ntu.edu.tw/~sylee/courses/clips/rhs.htm

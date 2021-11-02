#lang racket
(provide (all-defined-out))
(require "TDAfecha.rkt")
; tda acceso, contructor
(define acceso(lambda (id fechaini fechafin)
                (if (and (fecha? fechaini)
                         (fecha? fechafin)
                         (integer? id))
                    ; caso verdadero
                    (list id fechaini fechafin)
                    ; caso falso
                    null
                    )
                )
  )
  
; pertenencia
(define acceso? (lambda (acceso)
                  (and (integer? (car acceso))
                       (fecha? (cadr acceso))
                       (fecha? (caddr acceso)
                               )
                       )
                  )
  )
 

; selectores
(define getid (lambda (acceso)
                (car acceso)
                )
  )
(define getfechaini(lambda (acceso)
                     (cadr acceso)
                     )
  )

(define getfechafin(lambda (acceso)
                     (caddr acceso)
                     )
  )

  
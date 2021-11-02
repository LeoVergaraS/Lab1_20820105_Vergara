#lang racket
(provide (all-defined-out))
; TDA FECHA:
; Constructor tda de fecha.
; dominio: 3 enteros positivos + {0}
; recorrido: Una lista
(define fecha(lambda (dia mes anio)
               (if (and (integer? dia)
                        (integer? mes)
                        (integer? anio)
                        )
                   ;Caso Verdadero
                   (list dia mes anio)
                   ;Caso Falso
                   null
                   )
               )
  )
; Pertenencia Tda fecha.
(define fecha? (lambda (fecha)
                 (and (integer? (car fecha))
                        (integer? (cadr fecha))
                        (integer? (caddr fecha))
                        )
                 )
  )

; Selectores tda fecha.
(define getDia car)
(define getMes(lambda(lista)
                (cadr lista)
                )
  )
(define getAnio(lambda(lista)
                 (caddr lista)
                 )
  )

; Modificadores tda lista
(define setDia(lambda(lista dia)
                (fecha dia
                       (getMes lista)
                       (getAnio lista))
                )
  )
(define setMes(lambda(lista mes)
                (fecha (getDia lista)
                       mes
                      (getAnio lista)
                      )
                )
  )
(define setAnio(lambda(lista anio)
                 (fecha (getDia lista)
                       (getMes lista)
                        anio)
                 )
  )
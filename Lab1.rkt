#lang racket
; TDA FECHA:
; Constructor tda de fecha.
; dominio: 3 enteros positivos + {0}
; recorrido: Una lista
(define fecha(lambda (dia mes anio) (list dia mes anio)))
; Selectores tda fecha.
(define getDia car)
(define getMes(lambda(lista)
                (car(cdr lista))))
(define getAnio(lambda(lista)
                 (car(cdr(cdr lista)))))
; Modificadores tda lista
(define setDia(lambda(lista dia)
                (list dia (getMes lista) (getAnio lista))))
(define setMes(lambda(lista mes)
                (list (getDia lista) mes (getAnio lista))))
(define setAnio(lambda(lista anio)
                 (list (getDia lista) (getMes lista) anio)))


; TDA DocsDuck
; Constructor de DocsDuck
(define DocsDuck(lambda (nombre fecha funcionEncriptado funcionDesencriptado)
                  (list nombre fecha funcionEncriptado funcionDesencriptado)))
; Selectores
(define getNombre car)
(define getFecha(lambda(lista)
                  (car(cdr lista))))
(define getFE(lambda(lista)
                  (car(cdr(cdr lista)))))
(define getFD(lambda(lista)
                  (car(cdr(cdr(cdr lista))))))





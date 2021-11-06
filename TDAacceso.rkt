#lang racket
(provide (all-defined-out))
(require "TDAfecha.rkt")
; tda acceso, contructor
(define acceso(lambda (usuario permiso)
                (if (and (string? usuario)
                         (char? permiso))
                    ; caso verdadero
                    (list usuario permiso)
                    ; caso falso
                    null
                    )
                )
  )
  
; pertenencia
(define acceso? (lambda (acceso)
                  (and   (string? (car acceso))
                         (char? (cadr acceso)))
                  ) 
  )
 

; selectores
(define getUsuario (lambda (acceso)
                (car acceso)
                )
  )
(define getPermiso(lambda (acceso)
                     (cadr acceso)
                     )
  )

  
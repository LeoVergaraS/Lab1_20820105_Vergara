#lang racket
(provide (all-defined-out))
(require "TDAfecha.rkt")

; contructor TDA historial
(define historial(lambda (id contenido usuario fecha)
                   (if(and(integer? id)
                          (string? contenido)
                          (string? usuario)
                          (fecha? fecha))
                      ; caso verdadero
                      (list id contenido usuario fecha)
                      ; caso falso
                      null
                      )
                   )
  )

; pertenencia
(define historial? (lambda(historial)
                     (and(integer? (car historial))
                         (string? (cadr historial))
                         (string? (caddr historial))
                         (fecha? (cadddr historial)))
                     )
  )

; selector
(define getId(lambda(historial)
               (car historial)
               )
  )
(define getVersionContenido(lambda(historial)
               (cadr historial)
               )
  )
(define getUsuarioModificador(lambda(historial)
               (caddr historial)
               )
  )
(define getFechaModificacion(lambda(historial)
               (cadddr historial)
               )
  )

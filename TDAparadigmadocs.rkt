#lang racket
(provide (all-defined-out))
(require "TDAfecha.rkt")
(require "TDAusuario.rkt")
; TDA DocsDuck
; Constructor de paradigmadocs
(define paradigmadocs(lambda (nombre fechacreacion funcionEncriptado funcionDesencriptado)
                  (if (and (string? nombre)
                           (fecha? fechacreacion)
                           (procedure? funcionEncriptado)
                           (procedure? funcionDesencriptado)
                           )
                      ; Caso verdadero
                      (list nombre fechacreacion funcionEncriptado funcionDesencriptado (list (usuario "admin" "adminpass" (fecha  1 11 2021))))
                      ; Caso Falso
                      null
                      )
                  )
  )

; Pertenencia de paradigmadocs
(define paradigmadocs?(lambda (paradigmadocs)
                   (and (string? (car paradigmadocs))
                           (fecha? (cadr paradigmadocs))
                           (procedure? (caddr paradigmadocs))
                           (procedure? (cadddr paradigmadocs))
                         )
                   )
  )


; Selectores
(define getNombre(lambda (lista)
                   (car lista)
                   )
  )
(define getFecha(lambda(lista)
                  (cadr lista)
                  )
  )
(define getFE(lambda(lista)
                  (caddr lista)
               )
  )
(define getFD(lambda(lista)
                  (cadddr lista)
               )
  )
(define getListaUsuario(lambda(lista)
                         (cadddr(cdr lista))
                         )
  )

; modificadores
(define setListaUsuario(lambda (DuckDocs lista)
                         (list (getNombre DuckDocs)
                                        (getFecha DuckDocs)
                                        (getFE DuckDocs)
                                        (getFD DuckDocs)
                                        lista
                                        )
                         )
  )
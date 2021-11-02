#lang racket
(provide (all-defined-out))
(require "TDAdocumento.rkt")

; Contructor TDA usuario
(define usuario(lambda (nombreusuario contrasenia)
                  (if (and (string? nombreusuario)
                           (string? contrasenia)
                           )
                      ; caso verdadero
                      (list nombreusuario contrasenia (list (documento "" "")))
                      ; caso falso
                      null
                      )
                  )
  )

; pertenencia
(define usuario?(lambda (usuario)
                   (and (string? (car usuario))
                        (string? (cadr usuario))
                       )
                   )
  )

; selectores
(define getNombreusuario (lambda (usuario)
                           (car usuario)
                           )
  )

(define getContrasenia (lambda (usuario)
                         (cadr usuario)
                         )
  )

(define getListaDocumentos (lambda (usuario)
                             (caddr usuario)
                             )
  )

; modificador
(define setNombreusuario (lambda (usuario nombreusuario)
                           (usuario  nombreusuario
                                    (getContrasenia usuario)
                                    (getListaDocumentos usuario))
                           )
  )
(define setContrasenia (lambda (usuario contrasenia)
                           (usuario (getNombreusuario usuario)
                                     contrasenia
                                    (getListaDocumentos usuario))
                           )
  )
(define setListaDocumentos (lambda (usuario lista)
                           (usuario (getNombreusuario usuario)
                                    (getContrasenia usuario)
                                    lista
                                    )
                           )
  )

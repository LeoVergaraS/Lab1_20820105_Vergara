#lang racket
(provide (all-defined-out))
(require "TDAdocumento.rkt")
(require "TDAacceso.rkt")
(require "TDAfecha.rkt")

; Contructor TDA usuario
(define usuario(lambda (nombreusuario contrasenia fecharegistro)
                  (if (and (string? nombreusuario)
                           (string? contrasenia)
                           (fecha? fecharegistro)
                           )
                      ; caso verdadero
                      (list nombreusuario contrasenia fecharegistro (list (documento 0 "" "")) (list(acceso 0 (fecha 0 0 0) (fecha 0 0 0))))
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
(define getfecha (lambda (usuario)
                         (caddr usuario)
                         )
  )

(define getListaDocumentos (lambda (usuario)
                             (cadddr usuario)
                             )
  )
(define getlistaacceso (lambda (usuario)
                             (cadddr (cdr usuario))
                             )
  )

; modificador
(define setNombreusuario (lambda (usuario nombreusuario)
                           (list  nombreusuario
                                    (getContrasenia usuario)
                                    (getfecha usuario)
                                    (getListaDocumentos usuario)
                                    (getlistaacceso usuario))
                           )
  )
(define setContrasenia (lambda (usuario contrasenia)
                           (list (getNombreusuario usuario)
                                     contrasenia
                                    (getfecha usuario)
                                    (getListaDocumentos usuario)
                                    (getlistaacceso usuario))
                           )
  )
(define setListaDocumentos (lambda (usuario lista)
                           (list (getNombreusuario usuario)
                                    (getContrasenia usuario)
                                    (getfecha usuario)
                                    lista
                                    (getlistaacceso usuario)
                                    )
                           )
  )

(define setAcceso (lambda (usuario acceso)
                           (list (getNombreusuario usuario)
                                    (getContrasenia usuario)
                                    (getfecha usuario)
                                    (getListaDocumentos usuario)
                                     acceso
                                    )
                           )
  )

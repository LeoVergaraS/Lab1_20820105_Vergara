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
                      (list nombreusuario contrasenia fecharegistro (list (documento "" (fecha 0 0 0) "" "")) "Desconectado")
                      ; caso falso
                      null
                      )
                  )
  )

; pertenencia
(define usuario?(lambda (usuario)
                   (and (string? (car usuario))
                        (string? (cadr usuario))
                        (fecha? (caddr usuario))
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
(define getestado(lambda (usuario)
                             (cadddr (cdr usuario))
                             )
  )

; modificador
(define setNombreusuario (lambda (usuario nombreusuario)
                           (list  nombreusuario
                                    (getContrasenia usuario)
                                    (getfecha usuario)
                                    (getListaDocumentos usuario)
                                    (getestado usuario))
                           )
  )
(define setContrasenia (lambda (usuario contrasenia)
                           (list (getNombreusuario usuario)
                                     contrasenia
                                    (getfecha usuario)
                                    (getListaDocumentos usuario)
                                    (getestado usuario))
                           )
  )
(define setListaDocumentos (lambda (usuario lista)
                           (list (getNombreusuario usuario)
                                    (getContrasenia usuario)
                                    (getfecha usuario)
                                    lista
                                    (getestado usuario)
                                    )
                           )
  )

(define setEstado (lambda (usuario estado)
                           (list (getNombreusuario usuario)
                                    (getContrasenia usuario)
                                    (getfecha usuario)
                                    (getListaDocumentos usuario)
                                     estado
                                    )
                           )
  )
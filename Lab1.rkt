#lang racket
(require "TDAfecha.rkt")
(require "TDAparadigmadocs.rkt")
(require "TDAdocumento.rkt")
(require "TDAusuario.rkt")

(define DuckDocsVacio(paradigmadocs "DuckDocs" (fecha 1 11 2021) "Fe" "Fd"))

; Register
(define verificarUsuario(lambda (listaUsuario usuario)
                          (if (null? listaUsuario)
                              ; caso verdadero
                              #f
                              ; caso falso
                              (if (equal? (getNombreusuario(car listaUsuario)) usuario)
                                  ; caso verdadero
                                  #t
                                  ; caso falso
                                  (verificarUsuario (cdr listaUsuario) usuario)
                                  )
                              )
                          )
  )

(define actualizarLista(lambda (listaUsuario nombreusuario contrasenia)
                         (if (null? listaUsuario)
                             ; caso verdadero
                             (cons (usuario nombreusuario contrasenia) null)
                             ; caso falso
                             (cons (car listaUsuario) (actualizarLista (cdr listaUsuario) nombreusuario contrasenia))
                             )
                         )
  )

(define register (lambda (paradigmadocs fecha nombreusuario contrasenia)
                  (if (verificarUsuario (getListaUsuario paradigmadocs) nombreusuario)
                      ; caso verdadero
                      paradigmadocs
                      ; caso falso
                      (setListaUsuario paradigmadocs (actualizarLista (getListaUsuario paradigmadocs) nombreusuario contrasenia))
                      )
                     )
  )


(define gDocs1
(register (register (register DuckDocsVacio (fecha 25 10 2021) "user1" "pass1") (fecha 25 10 2021) "user2"
"pass2") (fecha 25 10 2021) "user3" "pass3"))

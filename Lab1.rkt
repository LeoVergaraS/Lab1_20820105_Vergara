#lang racket
(require "TDAfecha.rkt")
(require "TDAparadigmadocs.rkt")
(require "TDAdocumento.rkt")
(require "TDAusuario.rkt")
(require "TDAacceso.rkt")

(define DuckDocsVacio(paradigmadocs "DuckDocs" (fecha 1 11 2021) "Fe" "Fd"))

; Register
(define equalUser?(lambda (listaUsuario usuario)
                          (if (null? listaUsuario)
                              ; caso verdadero
                              #f
                              ; caso falso
                              (if (equal? (getNombreusuario(car listaUsuario)) usuario)
                                  ; caso verdadero
                                  #t
                                  ; caso falso
                                  (equalUser? (cdr listaUsuario) usuario)
                                  )
                              )
                          )
  )

(define actualizarLista(lambda (listaUsuario nombreusuario contrasenia fecha)
                         (if (null? listaUsuario)
                             ; caso verdadero
                             (cons (usuario nombreusuario contrasenia fecha) null)
                             ; caso falso
                             (cons (car listaUsuario) (actualizarLista (cdr listaUsuario) nombreusuario contrasenia fecha))
                             )
                         )
  )

(define register (lambda (paradigmadocs fecha nombreusuario contrasenia)
                  (if (equalUser? (getListaUsuario paradigmadocs) nombreusuario)
                      ; caso verdadero
                      paradigmadocs
                      ; caso falso
                      (setListaUsuario paradigmadocs (actualizarLista (getListaUsuario paradigmadocs) nombreusuario contrasenia fecha))
                      )
                     )
  )


(define gDocs1
(register (register (register DuckDocsVacio (fecha 25 10 2021) "user1" "pass1") (fecha 25 10 2021) "user2"
"pass2") (fecha 25 10 2021) "user3" "pass3"))


; login
(define des->con(lambda(listausuario nombreusuario)
                  (if (null? listausuario)
                      ; caso verdadero
                      null
                      ; caso falso
                      (if (equal? (getNombreusuario (car listausuario)) nombreusuario)
                          ; caso verdadero
                          (cons (setEstado (car listausuario) "Conectado") (des->con (cdr listausuario) nombreusuario))
                          ; caso falso
                          (cons (car listausuario) (des->con (cdr listausuario) nombreusuario))
                          )
                      )
                  )
  )

(define posicionUsuario(lambda(lista usuario i)
                     (if (equal? (getNombreusuario(car lista)) usuario)
                         ; caso verdadero
                         i
                         ; caso falso
                         (posicionUsuario (cdr lista) usuario (+ i 1))
                         )
                         )
  )
(define login(lambda(paradigmadocs nombreusuario contrasenia funcion)
               (if (equalUser? (getListaUsuario paradigmadocs) nombreusuario)
                   ; caso verdadero
                   (if (equal? (getContrasenia(list-ref (getListaUsuario paradigmadocs) (posicionUsuario (getListaUsuario paradigmadocs) nombreusuario 0))) contrasenia)
                       ; caso verdadero
                       (funcion (setListaUsuario paradigmadocs (des->con (getListaUsuario paradigmadocs) nombreusuario)))
                       ; caso falso
                       (funcion paradigmadocs)
                       )
                   ; caso falso
                   (funcion paradigmadocs)
               )
  ))



; create
(define conectado? (lambda (listausuarios)
                     (if (null? listausuarios)
                         ; caso verdadero
                         #f
                         ; caso falso
                         (if (equal? (getestado (car listausuarios)) "Conectado")
                             ; caso verdadero
                             #t
                             ; caso falso
                             (conectado? (cdr listausuarios))
                             )
                         )
                     )
  )

(define buscarConectado(lambda(lista)
                     (if (equal? (getestado(car lista)) "Conectado")
                         ; caso verdadero
                         (car lista)
                         ; caso falso
                         (buscarConectado (cdr lista))
                         )
                         )
  )

(define agregarDocumento(lambda (lista autor fecha nombre contenido)
                          (if (null? lista)
                              ; caso verdadero
                              (cons (documento autor fecha nombre contenido) null)
                              ; caso falso
                              (agregarDocumento (cdr lista) autor fecha nombre contenido)
                              )
                          )
  )

(define agregaLista(lambda(lista usuario)
                     (if (null? lista)
                         ; caso verdadero
                         null
                         ; caso falso
                        (if (equal? (getestado(car lista)) "Conectado")
                            ; caso verdadero
                            (cons usuario (agregaLista (cdr lista) usuario))
                            ; caso falso
                            (cons (car lista) (agregaLista (cdr lista) usuario))
                         )
                     )
  ))


(define create(lambda (paradigmadocs)(lambda(fecha nombre contenido)
                                       (if (conectado? (getListaUsuario paradigmadocs))
                                           ; caso verdadero
                                           (setListaUsuario paradigmadocs
                                                            (agregaLista (getListaUsuario paradigmadocs)
                                                                         (setEstado (setListaDocumentos(buscarConectado(getListaUsuario paradigmadocs))
                                                                           (agregarDocumento(getListaDocumentos
                                                                                             (buscarConectado
                                                                                              (getListaUsuario paradigmadocs)))
                                                                                            (getNombreusuario
                                                                                             (buscarConectado
                                                                                              (getListaUsuario paradigmadocs)))
                                                                                            fecha
                                                                                            nombre
                                                                                            contenido)
                                                              ) "Desconectado")
                                                        ))
                                           ; caso falso
                                           paradigmadocs
                                           )
                                       )
                )
  )

(define gDocs3 ((login gDocs1 "user2" "pass2" create) (fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs4 ((login gDocs3 "user1" "pass1" create) (fecha 30 08 2021) "doc1" "contenido doc1"))

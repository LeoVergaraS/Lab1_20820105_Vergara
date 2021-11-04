#lang racket
(require "TDAfecha.rkt")
(require "TDAparadigmadocs.rkt")
(require "TDAdocumento.rkt")
(require "TDAusuario.rkt")
(require "TDAacceso.rkt")

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
                              (cons (car lista) (agregarDocumento (cdr lista) autor fecha nombre contenido))
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
                                       ; verifico si hay un usuario conectado.
                                       (if (conectado? (getListaUsuario paradigmadocs))
                                           ; caso verdadero, actualizo la lista de usuario.
                                           (setListaUsuario
                                            ; Le paso paradigmadocs
                                            paradigmadocs
                                            ; y la lista actualizada. Para ello se agrega
                                            ; el usuario actualizado a la lista.
                                            (agregaLista
                                             ; Le paso la lista de usuarios.
                                             (getListaUsuario paradigmadocs)
                                             ; y el usuario actualizado. Para actualizar al usuario,
                                             ; primero se agrega el documento y luego se actualiza
                                             ; el estado del usuario.
                                             (setEstado
                                              ; Para el estado, le paso al usuario con la lista documento actualizada.
                                              (setListaDocumentos
                                               ; para la lista de documento, le paso el usuario.
                                               (buscarConectado(getListaUsuario paradigmadocs))
                                               ; y le paso la lista de documentos actualizada.
                                               (agregarDocumento
                                                ; para ello se agrega el nuevo documento a la lista de documento de ese usuario.
                                                ; Le paso la lista de documentos del usuario.
                                                (getListaDocumentos(buscarConectado(getListaUsuario paradigmadocs)))
                                                ; el nombre del usuario (autor)
                                                (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                ; la fecha de creacion.
                                                fecha
                                                ; el nombre del archivo
                                                nombre
                                                ; y el conetnido
                                                ((getFE paradigmadocs) contenido)))
                                              ; y el nuevo estado.
                                              "Desconectado")
                                                        ))
                                           ; caso falso, no altero la plataforma.
                                           paradigmadocs
                                           )
                                       )
                )
  )

; funcion encriptado y desencriptado.
(define cambiarLetras(lambda (lista)
                       (if (null? lista)
                           ; caso verdadero
                           null
                           ; caso falso
                           (cond
                             [(equal? #\a (car lista))(cons #\ψ (cambiarLetras (cdr lista)))]
                             [(equal? #\e (car lista))(cons #\ω (cambiarLetras (cdr lista)))]
                             [(equal? #\i (car lista))(cons #\ζ (cambiarLetras (cdr lista)))]
                             [(equal? #\o (car lista))(cons #\ξ (cambiarLetras (cdr lista)))]
                             [(equal? #\u (car lista))(cons #\ρ (cambiarLetras (cdr lista)))]
                             [(equal? #\ψ (car lista))(cons #\a (cambiarLetras (cdr lista)))]
                             [(equal? #\ω (car lista))(cons #\e (cambiarLetras (cdr lista)))]
                             [(equal? #\ζ (car lista))(cons #\i (cambiarLetras (cdr lista)))]
                             [(equal? #\ξ (car lista))(cons #\o (cambiarLetras (cdr lista)))]
                             [(equal? #\ρ (car lista))(cons #\u (cambiarLetras (cdr lista)))]
                             [else (cons (car lista) (cambiarLetras (cdr lista)))])
                           )
                       )
  )

(define encryptFunction (lambda (contenido)
                          (list->string(cambiarLetras (string->list contenido)))
                          )
  )

(define DecryptFunction (lambda (contenido)
                          (list->string(cambiarLetras (string->list contenido)))
                          )
  )

(define DuckDocsVacio(paradigmadocs "DuckDocs" (fecha 1 11 2021) encryptFunction DecryptFunction))
(define gDocs1
(register (register (register DuckDocsVacio (fecha 25 10 2021) "user1" "pass1") (fecha 25 10 2021) "user2"
"pass2") (fecha 25 10 2021) "user3" "pass3"))
(define gDocs3 ((login gDocs1 "user2" "pass2" create) (fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs4 ((login gDocs3 "user2" "pass2" create) (fecha 30 08 2021) "doc1" "contenido doc1"))

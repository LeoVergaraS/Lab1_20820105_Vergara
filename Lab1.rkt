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
                                            (setListaDocumentos
                                             paradigmadocs
                                            (agregarDocumento
                                             (getListaDocumentos paradigmadocs)
                                             (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                             fecha
                                             nombre
                                             ((getFE paradigmadocs) contenido)))
                                            (agregaLista
                                             (getListaUsuario paradigmadocs)
                                             (setEstado
                                              (buscarConectado(getListaUsuario paradigmadocs))
                                              "Desconectado")))
                                           ; caso falso, no altero la plataforma.
                                           paradigmadocs
                                           )
                                       )
                
                )
  )

; share
(define tienePermiso? (lambda (listaPermiso usuario)
                        (if (null? listaPermiso)
                            ; caso verdadero
                            #f
                            ; caso falso
                            (if (equal? (getUsuario (car listaPermiso)) usuario)
                                ; caso verdadero
                                #t
                                ; caso falso
                                (tienePermiso? (cdr listaPermiso) usuario)
                                )
                            )
                        )
  )

(define agregarListaPermiso(lambda (listaPermiso accesses aux)
                             (if (null? accesses)
                                 ; caso verdadero
                                 (if (null? listaPermiso)
                                     ; caso verdadero
                                     aux
                                     ; caso falso
                                     (if (tienePermiso? aux (getUsuario (car listaPermiso)))
                                         ; caso verdadero
                                         (agregarListaPermiso (cdr listaPermiso) null aux)
                                         ; caso falso
                                         (agregarListaPermiso (cdr listaPermiso) null (cons (car listaPermiso) aux))
                                         )
                                     )
                                 ; caso falso
                                 (agregarListaPermiso listaPermiso (cdr accesses) (cons (car accesses) aux))
                                 )
                             )
  )

(define actualizarListaDocumentos(lambda (lista documento)
                               (if (null? lista)
                                   ; caso verdadero
                                   null
                                   ; caso falso
                                   (if (and(equal? (getNombreDocumento documento) (getNombreDocumento (car lista)))
                                           (equal? (getAutor documento) (getAutor (car lista))))
                                       ; caso verdadero
                                       (cons documento (actualizarListaDocumentos (cdr lista) documento))
                                       ; caso falso
                                       (cons (car lista) (actualizarListaDocumentos (cdr lista) documento))
                                       )
                                   )
                               )
  )

(define contarDocumentos(lambda (listaDocumentos usuario i)
                          (if (null? listaDocumentos)
                              ; caso verdadero
                              i
                              ; caso falso
                              (if (equal? (getAutor(car listaDocumentos)) usuario)
                                  ; caso verdadero
                                  (contarDocumentos (cdr listaDocumentos) usuario (+ i 1))
                                  ; caso falso
                                  (contarDocumentos (cdr listaDocumentos) usuario i)
                                  )
                              )
                          )
  )


(define share(lambda(paradigmadocs)(lambda(idDoc . accesses)
               (if (conectado? (getListaUsuario paradigmadocs))
                   ; caso verdadero
                   (if (>= (contarDocumentos (getListaDocumentos paradigmadocs) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))) 0) idDoc)
                       ; caso verdadero
                       (setListaUsuario
                        (setListaDocumentos
                         paradigmadocs
                         (actualizarListaDocumentos
                          (getListaDocumentos paradigmadocs)
                          (setListaPermiso
                           (list-ref (filter (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado (getListaUsuario paradigmadocs)))))(getListaDocumentos paradigmadocs))(- idDoc 1))
                           (agregarListaPermiso
                            (getListaPermiso
                             (list-ref (filter (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado (getListaUsuario paradigmadocs)))))(getListaDocumentos paradigmadocs))(- idDoc 1)))
                             accesses
                             null))))
                        (agregaLista
                         (getListaUsuario paradigmadocs)
                         (setEstado
                          (buscarConectado(getListaUsuario paradigmadocs))
                          "Desconectado")))
                       ; caso falso
                       (setListaUsuario paradigmadocs (agregaLista (getListaUsuario paradigmadocs) (setEstado (buscarConectado(getListaUsuario paradigmadocs)) "Desconectado")))
                       )
                   ; caso falso
                   paradigmadocs
                                           )
                                     )
               )
  )


; add
; restoreVersion
; funcion revokeAllAccesses
(define sacarPermisos (lambda (documento usuario)
                        (if (equal? (getAutor documento) usuario)
                            ; caso verdadero
                            (setListaPermiso documento null)
                            ; caso falso
                            documento)
                        )
  )

(define revokeAllAccesses(lambda (paradigmadocs)
                           (if (conectado? (getListaUsuario paradigmadocs))
                               ; caso verdadero
                               (if (not(null? (filter (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado (getListaUsuario paradigmadocs)))))(getListaDocumentos paradigmadocs))))
                                   ; caso verdadero
                                      (setListaDocumentos
                                        paradigmadocs
                                       (map
                                        (lambda(documento)(sacarPermisos documento (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                        (getListaDocumentos paradigmadocs)))
                                   ; caso falso
                                   (setListaUsuario
                                    paradigmadocs
                                    (agregaLista
                                     (getListaUsuario paradigmadocs)
                                     (setEstado
                                      (buscarConectado(getListaUsuario paradigmadocs))
                                      "Desconectado")))
                                   )
                               
                               ; caso falso
                               paradigmadocs
                               )
                           )
  )

; search
; paradigmadocs->string

; (opcionales)
; delete
; searchAndReplace
; applyStyles
; comment
; ctrlZ y ctrlY


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


(define gDocs2 ((login gDocs1 "user1" "pass1" create) (fecha 30 08 2021) "doc3" "contenido doc3"))
(define gDocs3 ((login gDocs2 "user2" "pass2" create) (fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs4 ((login gDocs3 "user2" "pass2" create) (fecha 30 08 2021) "doc1" "contenido doc1"))
(define gDocs5 ((login gDocs4 "user1" "pass1" create) (fecha 30 08 2021) "doc0" "contenido doc0"))
(define gDocs6 ((login gDocs5 "user2" "pass2" create) (fecha 30 08 2021) "doc5" "contenido doc5"))
(define gDocs7 ((login gDocs6 "user3" "pass3" create) (fecha 30 08 2021) "doc1" "contenido doc1"))
(define gDocs8 ((login gDocs7 "user2" "pass2" create) (fecha 30 08 2021) "doc10" "contenido doc10"))
(define gDocs9 ((login gDocs8 "user3" "pass3" create) (fecha 30 08 2021) "doc2" "contenido doc2"))


(define gDocs10  ((login gDocs9 "user2" "pass2" share) 4 (acceso "user1" #\r) (acceso "user3" #\r) (acceso "user4" #\r)))

(define gDocs11 ((login gDocs10 "user2" "pass2" share) 2 (acceso "user1" #\c)))
(define gDocs12 (login gDocs11 "user2" "pass2" revokeAllAccesses))
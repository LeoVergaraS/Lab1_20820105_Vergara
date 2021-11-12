#lang racket

(require "TDAfecha.rkt")
(require "TDAparadigmadocs.rkt")
(require "TDAdocumento.rkt")
(require "TDAusuario.rkt")
(require "TDAacceso.rkt")
(require "TDAhistorial.rkt")

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
(define tienePermisoEscritura? (lambda(listaPermiso usuario)
                                 (if (null? listaPermiso)
                                     ; caso verdadero
                                     #f
                                     ; caso falso
                                     (if (and(equal? (getUsuario (car listaPermiso)) usuario)
                                             (equal? (getPermiso (car listaPermiso)) #\w))
                                         ; caso verdadero
                                         #t
                                         ; caso falso
                                         (tienePermisoEscritura? (cdr listaPermiso) usuario)
                                         )
                                     )
                                 )
  )

(define agregarListaHistorial(lambda(listaHistorial i contenido usuario fecha)
                               (if (null? listaHistorial)
                                   ; caso verdadero
                                   (cons (historial i contenido usuario fecha) null)
                                   ; caso falso
                                   (cons (car listaHistorial)(agregarListaHistorial (cdr listaHistorial) (+ i 1) contenido usuario fecha))
                                   )
                               )
  )

(define add(lambda (paradigmadocs)(lambda (idDoc fecha contenidoTexto)
                                    (if (conectado? (getListaUsuario paradigmadocs))
                                        ; caso verdadero
                                        (if (or (equal? (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                                (tienePermisoEscritura? (getListaPermiso (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                            ; caso verdadero
                                            (setListaUsuario (setListaDocumentos paradigmadocs (actualizarListaDocumentos (getListaDocumentos paradigmadocs)(setContenido (setListaHistorial (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1)) (agregarListaHistorial (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) 0 (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))) fecha))(string-append (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) " " ((getFE paradigmadocs) contenidoTexto))))) (agregaLista (getListaUsuario paradigmadocs) (setEstado (buscarConectado(getListaUsuario paradigmadocs)) "Desconectado")))
                                            
                                            ; caso falso
                                            (setListaUsuario paradigmadocs (agregaLista (getListaUsuario paradigmadocs) (setEstado (buscarConectado(getListaUsuario paradigmadocs)) "Desconectado")))
                                            )
                                        ; caso falso
                                        paradigmadocs
                                        )
                                    )
             )
  )
; restoreVersion
(define restoreVersion(lambda (paradigmadocs)(lambda(idDoc idVersion)
                                               (if (conectado? (getListaUsuario paradigmadocs))
                                                   ; caso verdadero
                                                   (if (equal? (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                                       ; caso verdadero
                                                       (setListaUsuario
                                                        (setListaDocumentos
                                                         paradigmadocs
                                                         (actualizarListaDocumentos
                                                          (getListaDocumentos paradigmadocs)
                                                          (setContenido
                                                           (setListaHistorial
                                                            (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))
                                                            (agregarListaHistorial
                                                             (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                             0
                                                             (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                             (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                             (getFechaModificacion(list-ref(getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) idVersion))))
                                                            (getContenidoVersion(list-ref(getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) idVersion)))))
                                                        (agregaLista (getListaUsuario paradigmadocs) (setEstado (buscarConectado(getListaUsuario paradigmadocs)) "Desconectado")))
                                                       ; caso falso
                                                       (setListaUsuario paradigmadocs (agregaLista (getListaUsuario paradigmadocs) (setEstado (buscarConectado(getListaUsuario paradigmadocs)) "Desconectado")))
                                                       )
                                                   ; caso falso
                                                   paradigmadocs
                                                   )
                                               )
                        )
  )
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
                                      (setListaUsuario (setListaDocumentos
                                        paradigmadocs
                                       (map
                                        (lambda(documento)(sacarPermisos documento (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                        (getListaDocumentos paradigmadocs))) (agregaLista
                                     (getListaUsuario paradigmadocs)
                                     (setEstado
                                      (buscarConectado(getListaUsuario paradigmadocs))
                                      "Desconectado")))
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
(define tienePermisoLectura?(lambda(listaPermiso usuario)
                              (if (null? listaPermiso)
                                     ; caso verdadero
                                     #f
                                     ; caso falso
                                     (if (and(equal? (getUsuario (car listaPermiso)) usuario)
                                             (equal? (getPermiso (car listaPermiso)) #\r))
                                         ; caso verdadero
                                         #t
                                         ; caso falso
                                         (tienePermisoEscritura? (cdr listaPermiso) usuario)
                                         )
                                     )
                              )
  )
(define search-substring-historial(lambda(historial substring funciondesencriptado)
                          (if (null? historial)
                              ; caso verdadero
                              #f
                              ; caso falso
                              (if (string-contains? (funciondesencriptado (getContenidoVersion(car historial))) substring)
                                  ; caso verdadero
                                  #t
                                  ; caso falso
                                  (search-substring-historial (cdr historial) substring funciondesencriptado)
                                  )
                              )
                          )
  )

(define filtroSearch(lambda (documento nombreusuario texto FD)
                      (if(or(equal? (getAutor documento) nombreusuario)
                         (tienePermisoEscritura? (getListaPermiso documento) nombreusuario)
                         (tienePermisoLectura? (getListaPermiso documento) nombreusuario))
                         ; caso verdadero
                         (or(string-contains? (FD (getContenido documento)) texto)
                            (search-substring-historial (getListaHistorial documento) texto FD))
                         ; caso falso
                         #f
                         )
                      )
  )

(define search(lambda(paradigmadocs)(lambda (texto)
                                      (if (conectado? (getListaUsuario paradigmadocs))
                                          ; caso verdadero
                                          (filter (lambda(documento)(filtroSearch documento (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))) texto (getFD paradigmadocs))) (getListaDocumentos paradigmadocs))
                                          ; caso falso
                                          null)
                                      )
                )
  )
; paradigmadocs->string
(define documento->string(lambda(documento FD)
                           (string-append
                            "Nombre del texto: "
                            (getNombreDocumento documento)
                            ". Autor: "
                            (getAutor documento)
                            ". Fecha de creacion: "
                            (fecha->string(getFechaCreacion documento))
                            ". Contenido:"
                            "\""
                            (FD (getContenido documento))
                            "\""

                            )
                           )
  )

(define listaDocumento->string(lambda(lista string i FD)
                                (if (null? lista)
                                    ; caso verdadero
                                    string
                                    ; caso falso
                                    (listaDocumento->string (cdr lista) (string-append string "  "(number->string i) ". " (documento->string (car lista) FD) "\n") (+ i 1) FD)
                                    )
                                )
  )

(define fecha->string(lambda(fecha)
                       (string-append
                        (number->string (getDia fecha))
                        "-"
                        (number->string (getMes fecha))
                        "-"
                        (number->string (getAnio fecha))
                        )
                       )
  )

(define paradigmadocs->string(lambda(paradigmadocs)
                               (if (conectado? (getListaUsuario paradigmadocs))
                                   ; caso verdadero
                                   (string-append
                                    "DUCKDOCS:\n Bienvenido "
                                    (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                    " (fecha de creacion: "
                                    (fecha->string (getfecha(buscarConectado(getListaUsuario paradigmadocs))))
                                    "). Su lista de documentos es:\n"
                                    (listaDocumento->string (filter (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))) (getListaDocumentos paradigmadocs))"" 1 (getFD paradigmadocs))
                                    "\n Los documentos que han sido compartido con su cuenta son:\n"
                                    (listaDocumento->string (filter (lambda(documento)(tienePermiso? (getListaPermiso documento) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))) (getListaDocumentos paradigmadocs)) "" 1 (getFD paradigmadocs))
                                    "\n\n Plataforma creada por Leo Vergara Sepulveda. Fecha de creacion: "
                                    (fecha->string (getFecha paradigmadocs))
                                    ". DuckDocs©."
                                    )
                                   ; caso falso
                                   (string-append
                                    "DUCKDOCS:\n Los documentos presente en la plataforma DuckDocs son:\n"
                                    (listaDocumento->string (getListaDocumentos paradigmadocs) "" 1 (getFD paradigmadocs))
                                    "\n\n Plataforma creada por Leo Vergara Sepulveda. Fecha de creacion: "
                                    (fecha->string (getFecha paradigmadocs))
                                    ". DuckDocs©."
                                    )
                                   )
                               )
  )

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


(define gDocs10  ((login gDocs9 "user2" "pass2" share) 4 (acceso "user1" #\w) (acceso "user3" #\r) (acceso "user4" #\r)))

(define gDocs11 ((login gDocs10 "user2" "pass2" share) 2 (acceso "user1" #\c)))
(define gDocs12 ((login gDocs11 "user1" "pass1" add) 3 (fecha 8 11 2021) "mas contenido para el texto"))
(define gDocs13 ((login gDocs12 "user1" "pass1" add) 3 (fecha 9 11 2021) "aun mas contenido"))
(define gDocs14 ((login gDocs13 "user1" "pass1" search) "contenido"))
(define gDocs15 (login gDocs13 "user1" "pass1" paradigmadocs->string))
;(define gDocs12 (login gDocs11 "user2" "pass2" revokeAllAccesses))

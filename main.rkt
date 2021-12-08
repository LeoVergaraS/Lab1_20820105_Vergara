#lang racket

(require "TDAfecha.rkt")
(require "TDAparadigmadocs.rkt")
(require "TDAdocumento.rkt")
(require "TDAusuario.rkt")
(require "TDAacceso.rkt")
(require "TDAhistorial.rkt")

; (Obligatorias)
; Register

; Descripcion: funcion que verifica si existe un nombre de usuario en la lista de usuario.
; Dominio: Lista TDA usuario X string
; Recorrido: booleano
(define equalUser?(lambda (listaUsuario usuario)
                    ; si se llego al final,
                    (if (null? listaUsuario)
                        ; caso verdadero: retorno un False debido a que no se
                        ; encontro el nombre de usuario en la lista.
                        #f
                        ; caso falso: Se verifica si es igual el nombre de usuario de
                        ; la cabeza de la lista al nombre entregado.
                        (if (equal? (getNombreusuario(car listaUsuario)) usuario)
                            ; caso verdadero: si son iguales se retorna un True
                            #t
                            ; caso falso: si no si revisando con la cola de la lista.
                            (equalUser? (cdr listaUsuario) usuario)
                            )
                        )
                    )
  )

; Descripcion: funcion que actualiza la lista de usuario de paradigmadocs.
;              Retorna la lista de TDA usuario, donde el ultimo elemento es el
;              nuevo usuario registrado. Se utiliza recursion natural.
; Dominio: Lista TDA usuario X string X string X TDA fecha
; Recorrido: Lista TDA usuario
(define actualizarLista(lambda (listaUsuario nombreusuario contrasenia fecha)
                         ; Si no hay mas elementos en la lista de usuarios,
                         (if (null? listaUsuario)
                             ; caso verdadero: agrego el nuevo usuario.
                             (cons (usuario nombreusuario contrasenia fecha) null)
                             ; caso falso: copio cada elemento de la lista de usuario.
                             (cons (car listaUsuario) (actualizarLista (cdr listaUsuario) nombreusuario contrasenia fecha))
                             )
                         )
  )


; Descripción: función que registra un usuario en paradigmadocs,
;              si el nombre de usuario existe en paradigmadocs no se registra.
;              Retorna paradigmadocs con el usuario registrado.
;              Se utiliza recursion natural en la funcion actualizarLista.
; Dominio: TDA paradigmadocs X TDA fecha X string X string
; Recorrido: TDA paradigmadocs
(define register (lambda (paradigmadocs fecha nombreusuario contrasenia)
                   ; Si el usuario existe en la lista de usuario de paradigmadocs,
                   ; se deja igual paradigmadocs. En caso contrario se agrega el nuevo
                   ; usuario a la lista.
                   (if (equalUser? (getListaUsuario paradigmadocs) nombreusuario)
                       ; caso verdadero: retorno paradigmadocs
                       paradigmadocs
                       ; caso falso: se actualiza la lista de usuario. Se utiliza el
                       ; modificador para la lista de usuario del TDA paradigmadocs.
                       (setListaUsuario
                        ; se le pasa paradigmadocs
                        paradigmadocs
                        ; y la lista actualizada.
                        (actualizarLista
                         ; Le paso la lista de usuario.
                         (getListaUsuario paradigmadocs)
                         ; Le paso los datos para crear el TDA usuario del nuevo usuario.
                         nombreusuario
                         contrasenia
                         fecha)
                       )
                      )
                   )
  )





; login

; Descripcion: funcion que actualiza la lista de usuario, copiando sus 
;              elementos y alterando el estado del usuario que tiene el 
;              mismo nombre al entregado. Retorna la lista de usuario. 
;              Se utiliza recursion natural.
; Dominio: Lista TDA usuario X string
; Recorrido: Lista TDA usuario.
(define des->con(lambda(listausuario nombreusuario)
                  ; Se verifica si es vacia la lista,
                  (if (null? listausuario)
                      ; caso verdadero: retorna un null.
                      null
                      ; caso falso: se verifica si el nombre de usuario
                      ; de la cabeza de la lista es igual al entregado,
                      (if (equal? (getNombreusuario (car listausuario)) nombreusuario)
                          ; caso verdadero: cambio el estado del usuario, y lo agrego    
                          (cons (setEstado (car listausuario) "Conectado") (des->con (cdr listausuario) nombreusuario))
                          ; caso falso: agrego la cabeza de la lista.
                          (cons (car listausuario) (des->con (cdr listausuario) nombreusuario))
                          )
                      )
                  )
  )

; Descripcion: funcion que busca el usuario en la lista de usuarios.
;              Retorna el usuario.
; Dominio: Lista TDA usuario X string
; Recorrido: TDA usuario
(define buscarUsuario(lambda(lista nombreusuario)
                       ; Se verifica si es igual el nombre de usuario ingresado
                       ; al nombre de usuario de la cabeza de la lista,
                       (if (equal? (getNombreusuario(car lista)) nombreusuario)
                           ; caso verdadero: retorno ese usuario
                           (car lista)
                           ; caso falso: Se busca con la cola de la lista.
                           (buscarUsuario (cdr lista) nombreusuario)
                           )
                       )
  )

; Descripcion: funcion que logea un usuario en paradigmadocs. Si existe el
;              usuario y la contraseña es correcta, se retorna la funcion
;              evaluada en paradigmadocs actualizando el estado del usuario.
;              Si no se retorna la funcion evaluada en paradigmadocs de entrada.
;              Se utilizo recursion natural con la funcion des->con.
; Dominio: TDA paradigmadocs X string X string X funcion
; Recorrido: Funcion X paradigmadocs
(define login(lambda(paradigmadocs nombreusuario contrasenia funcion)
               ; verifica si el usuario existen en paradigmadocs,
               (if (equalUser? (getListaUsuario paradigmadocs) nombreusuario)
                   ; caso verdadero: verifica si la contraseña es correcta,
                   (if (equal? (getContrasenia(buscarUsuario (getListaUsuario paradigmadocs) nombreusuario)) contrasenia)
                       ; caso verdadero: aplica la funcion en paradigmadocs
                       ; donde el usuario tenga el estado conectado.
                       (funcion
                        ; actualizo paradigmadocs. 
                        (setListaUsuario
                         ; para ello le paso el actual paradigmadocs.
                         paradigmadocs
                         ; y actualizo la lista de usuario, cambiando
                         ; el estado del usuario de desconectado a conectado.
                         (des->con (getListaUsuario paradigmadocs) nombreusuario)))
                       ; caso falso: retorno la funcion evaluada en paradigmadocs de entrada.
                       (funcion paradigmadocs)
                       )
                   ; caso falso: retorno la funcion evaluada en paradigmadocs de entrada.
                   (funcion paradigmadocs)
                   )
               )
  )



; create

; Descripcion: funcion que verifica si un usuario esta conectado.
;              Retorna un Booleano.
; Dominio: Lista TDA usuario.
; Recorrido: booleano.
(define conectado? (lambda (listausuarios)
                     ; Se verifica si la lista es vacia,
                     (if (null? listausuarios)
                         ; caso verdadero: significa que no se encontro
                         ; un usuario conectado, se retorna un False.
                         #f
                         ; caso falso: se verifica si el usuario de la
                         ; cabeza de la lista esta conectado,
                         (if (equal? (getestado (car listausuarios)) "Conectado")
                             ; caso verdadero: retorno un True
                             #t
                             ; caso falso: verifico con la cola de la lista.
                             (conectado? (cdr listausuarios))
                             )
                         )
                     )
  )

; Descripcion: funcion que busca el usuario conectado en una lista de usuarios.
;              Retorna el usuario conectado.
; Dominio: Lista TDA usuario
; Recorrido: TDA usuario
(define buscarConectado(lambda(lista)
                         ; Se verifica si el estado del usuario de
                         ; la cabeza de la lista es conectado,
                         (if (equal? (getestado(car lista)) "Conectado")
                             ; caso verdadero: retorno ese usuario.
                             (car lista)
                             ; caso falso: busco en la cola de la lista.
                             (buscarConectado (cdr lista))
                             )
                         )
  )

; Descripcion: funcion que agrega un documento en la lista de documentos de paradigmadocs.
;              Retorna la lista con el nuevo documento.
; Dominio: Lista TDA documento X string X TDA fecha X string X string 
; Recorrido: Lista TDA documento.
(define agregarDocumento(lambda (lista autor fecha nombre contenido)
                          ; Se verifica si la lista esta vacia,
                          (if (null? lista)
                              ; caso verdadero: agrego el nuevo documento.
                              (cons (documento autor fecha nombre contenido) null)
                              ; caso falso: agrego la cabeza de la lista y sigo con la cola.
                              (cons (car lista) (agregarDocumento (cdr lista) autor fecha nombre contenido))
                              )
                          )
  )

; Descripcion: funcion que cambia el usuario por el mismo usuario con el estado
;              en desconectado. Retorna la lista de usuario actualizada.
;              Se utiliza recursion natural.
; Dominio: Lista TDA usuario X TDA usuario.
; Recorrido: Lista TDA usuario.
(define agregaLista(lambda(lista usuario)
                     ; Se verifica si es vacia la lista,
                     (if (null? lista)
                         ; caso verdadero: se retorna null.
                         null
                         ; caso falso: se verifica si el estado del usuario 
                         ; de la cabeza de la lista es conectado,
                         (if (equal? (getestado(car lista)) "Conectado")
                             ; caso verdadero: agrego el usuario entregado.
                             (cons usuario (agregaLista (cdr lista) usuario))
                             ; caso falso: agrego la cabeza de la lista.
                             (cons (car lista) (agregaLista (cdr lista) usuario))
                             )
                         )
                     )
  ) 

; Descripcion: funcion que crea un archivo en paradigmadocs, para ello se 
;              debe estar logeado. Retorna paradigmadocs con el archivo creado.
;              Se utiliza recursion natural con agregaLista y agregarDocumento.
; Dominio: TDA paradigmadocs X TDA fecha X string X string
; Recorrido: TDA paradigmadocs
(define create(lambda (paradigmadocs)(lambda(fecha nombre contenido)
                                       ; Se verifica si hay un usuario conectado,
                                       (if (conectado? (getListaUsuario paradigmadocs))
                                           ; caso verdadero: Primero actualizo la lista de 
                                           ; documentos y luego desconecto al usuario.
                                           (setListaUsuario
                                            ; Se actualiza la lista de documentos,
                                            (setListaDocumentos
                                             ; se le pasa la actual plataforma
                                             paradigmadocs
                                             ; y agrego el nuevo documento a la lista
                                             ; de documentos.
                                             (agregarDocumento
                                              (getListaDocumentos paradigmadocs)
                                              (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                              fecha
                                              nombre
                                              ((getFE paradigmadocs) contenido)
                                              )
                                             )
                                            ; Cambio el estado del usuario a desconectado.
                                            (agregaLista
                                             (getListaUsuario paradigmadocs)
                                             (setEstado
                                              (buscarConectado(getListaUsuario paradigmadocs))
                                              "Desconectado")
                                             )
                                            )
                                           ; caso falso: no altero la plataforma.
                                           paradigmadocs
                                           )
                                       )
                
                )
  )

; share

; Descripcion: funcion que verifica si un usuario tiene permisos en una lista de
;              permisos. Retorna un booleano.
; Dominio: Lista TDA acceso X string
; Recorrido: Booleano.
(define tienePermiso? (lambda (listaPermiso usuario)
                        ; Se verifica si la lista de permiso esta vacia,
                        (if (null? listaPermiso)
                            ; caso verdadero: se retorna un False
                            #f
                            ; caso falso: se verifica si el usuario de la cabeza de 
                            ; la lista de permisos coincide con el usuario entregado,
                            (if (equal? (getUsuario (car listaPermiso)) usuario)
                                ; caso verdadero: se retorna un True
                                #t
                                ; caso falso: se continua revisando con la cola de la lista.
                                (tienePermiso? (cdr listaPermiso) usuario)
                                )
                            )
                        )
  )

; Descripcion: funcion que agrega o cambia los accesos de una lista de permiso
;              de un documento, primero copia accesses a aux y luego copia 
;              listaPermiso sin que se repitan permisos dado al mismo usuario.
;              Retorna la lista de permisos actualizada. Se utiliza recursion de cola.
; Dominio: Lista TDA acceso X TDA acceso X lista nula.
; Recorrido: Lista TDA acceso.
(define agregarListaPermiso(lambda (listaPermiso accesses aux)
                             ; Se verifica si la lista accesses es vacia,
                             (if (null? accesses)
                                 ; caso verdadero: se verifica si la lista de permiso es vacia,
                                 (if (null? listaPermiso)
                                     ; caso verdadero: se retorna aux.
                                     aux
                                     ; caso falso: se verifica si el usuario de la cabeza
                                     ; de la lista de permisos tiene permiso en aux.
                                     (if (tienePermiso? aux (getUsuario (car listaPermiso)))
                                         ; caso verdadero: se continua con la cola de la lista de permisos.
                                         (agregarListaPermiso (cdr listaPermiso) null aux)
                                         ; caso falso: se agrega la cabeza de la lista de permisos a aux
                                         ; y se continua con la cola.
                                         (agregarListaPermiso (cdr listaPermiso) null (cons (car listaPermiso) aux))
                                         )
                                     )
                                 ; caso falso: se copia la cabeza de accesses a aux,
                                 ; y se continua con la cola de accesses.
                                 (agregarListaPermiso listaPermiso (cdr accesses) (cons (car accesses) aux))
                                 )
                             )
  )

; Descripcion: funcion que actualiza una lista de documento cambiando
;              uno de sus elementos por el documento entregado. Retorna la 
;              lista de documentos actualizada. Se utiliza recursion natural.
; Dominio: Lista TDA documento X TDA documento.
; Recorrido: Lista TDA documento.
(define actualizarListaDocumentos(lambda (lista documento)
                                   ; se verifica si la lista esta vacia,
                                   (if (null? lista)
                                       ; caso verdadero: se retorna un null.
                                       null
                                       ; caso falso: se verifica si en el documento de la cabeza de la lista
                                       ; y el documento entregado, coincide el autor y el nombre del documento.
                                       (if (and
                                            ; mismo nombre del documento?
                                            (equal? (getNombreDocumento documento) (getNombreDocumento (car lista)))
                                            ; mismo autor?
                                            (equal? (getAutor documento) (getAutor (car lista))))
                                           ; caso verdadero: se agrega el documento entregado y se continua
                                           ; con la cola de la lista.
                                           (cons documento (actualizarListaDocumentos (cdr lista) documento))
                                           ; caso falso: se agrega la cabeza de la lista y se continua con
                                           ; la cola de la lista.
                                           (cons (car lista) (actualizarListaDocumentos (cdr lista) documento))
                                           )
                                       )
                                   )
  )

; Descripcion: funcion que cuenta los documentos de un usuario en la
;              plataforma paradigmadocs. Retorna un entero positivo.
;              Se utiliza recursion de cola.
; Dominio: Lista TDA documento X string X entero positivo.
; Recorrido: entero positivo.
(define contarDocumentos(lambda (listaDocumentos usuario i)
                          ; Se verifica si la lista de documentos esta vacia,
                          (if (null? listaDocumentos)
                              ; caso verdadero: se retorna i.
                              i
                              ; caso falso: se verifica si el autor del documento
                              ; de la cabeza de la lista es igual al usuario,
                              (if (equal? (getAutor(car listaDocumentos)) usuario)
                                  ; caso verdadero: se suma 1 a i y se continua con la cola de la lista.
                                  (contarDocumentos (cdr listaDocumentos) usuario (+ i 1))
                                  ; caso falso: se continua con la cola de la lista.
                                  (contarDocumentos (cdr listaDocumentos) usuario i)
                                  )
                              )
                          )
  )

; Descripcion: funcion que comparte un documento solo de su propiedad, dando
;              permisos a los demas usuarios. Para ello es necesario ocupar 
;              paradimadocs->string con login. Retorna paradigmadocs. Se utiliza
;              recursion de cola y natural.
; Dominio: TDA paradigmadocs X entero positivo X TDA acceso X ... X TDA acceso
; Recorrido: TDA paradigmadocs
(define share(lambda(paradigmadocs)(lambda(idDoc . accesses)
                                     ; Se verifica si hay un usuario conectado,
                                     (if (conectado? (getListaUsuario paradigmadocs))
                                         ; caso verdadero: Se verifica si el usuario
                                         ; tiene un documento con id = idDoc,
                                         (if (>= (contarDocumentos (getListaDocumentos paradigmadocs) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))) 0) idDoc)
                                             ; caso verdadero: Primero se agregan los
                                             ; permisos y luego se desconecta al usuario.
                                             (setListaUsuario
                                              ; se cambia la lista de documentos,
                                              (setListaDocumentos
                                               ; se pasa la actual plataforma
                                               paradigmadocs
                                               ; se actualiza la lista de documentos.
                                               (actualizarListaDocumentos
                                                ; se le pasa la actual lista de documentos.
                                                (getListaDocumentos paradigmadocs)
                                                ; se le pasa el documento id = idDoc con la
                                                ; lista de permisos cambiada.
                                                (setListaPermiso
                                                 ; se le pasa el documento con id = idDoc
                                                 ; de la lista filtrada.
                                                 (list-ref
                                                  ; se filtra la lista.
                                                  (filter
                                                   ; funcion de filtro.
                                                   (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado (getListaUsuario paradigmadocs)))))
                                                   ; lista a filtrar.
                                                   (getListaDocumentos paradigmadocs)
                                                   )
                                                  ; posicion del documento.
                                                  (- idDoc 1)
                                                  )
                                                 ; se le pasa la lista de permisos actualizada.
                                                 (agregarListaPermiso
                                                  ; para ello, se le pasa la lista de permiso del documento.
                                                  (getListaPermiso
                                                   (list-ref
                                                    (filter
                                                     (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado (getListaUsuario paradigmadocs)))))
                                                     (getListaDocumentos paradigmadocs)
                                                     )
                                                    (- idDoc 1)
                                                    )
                                                   )
                                                  ; se le pasa la lista de acceso.
                                                  accesses
                                                  ; y una lista nula.
                                                  null)
                                                 )
                                                )
                                               )
                                              ; ahora se desconecta al usuario.
                                              (agregaLista
                                               (getListaUsuario paradigmadocs)
                                               (setEstado
                                                (buscarConectado(getListaUsuario paradigmadocs))
                                                "Desconectado")
                                               )
                                              )
                                             ; caso falso: si no hay un documento con
                                             ; id = idDoc, se desconecta al usuario.
                                             (setListaUsuario
                                              paradigmadocs
                                              (agregaLista
                                               (getListaUsuario paradigmadocs)
                                               (setEstado
                                                (buscarConectado(getListaUsuario paradigmadocs))
                                                "Desconectado")
                                               )
                                              )
                                             )
                                         ; caso falso: si no hay un usuario
                                         ; conectado se retorna la plataforma.
                                         paradigmadocs
                                         )
                                     )
               )
  )


; add

; Descripcion: funcion que verifica si un usuario tiene permiso de escritura
;              sobre un documento. Retorna un booleano.
; Dominio: Lista TDA Acceso X string
; Recorrido: Booleano.
(define tienePermisoEscritura? (lambda(listaPermiso usuario)
                                 ; Se verifica si la lista esta vacia,
                                 (if (null? listaPermiso)
                                     ; caso verdadero: se retorna un False.
                                     #f
                                     ; caso falso: Se verifica si el acceso de la cabeza de la lista,
                                     ; tiene el mismo nombre del nombre de usuario entregado y
                                     ; si el permiso es de escritura, 
                                     (if (and(equal? (getUsuario (car listaPermiso)) usuario)
                                             (equal? (getPermiso (car listaPermiso)) #\w))
                                         ; caso verdadero: se retorna un True
                                         #t
                                         ; caso falso: se verifica con la cola de la lista.
                                         (tienePermisoEscritura? (cdr listaPermiso) usuario)
                                         )
                                     )
                                 )
  )

; Descripcion: funcion que agrega una version a la lista de historial.
;              Retorna la lista de historial actualizada. Se utiliza la recusrion natural.
; Dominio: Lista TDA Historial X entero positivo X string X string X TDA fecha.
; Recorrido: Lista TDA historial.
(define agregarListaHistorial(lambda(listaHistorial i contenido usuario fecha)
                               ; Se verifica si la lista esta vacia,
                               (if (null? listaHistorial)
                                   ; caso verdadero: agrego la nueva version.
                                   (cons (historial i contenido usuario fecha) null)
                                   ; caso falso: agrego la cabeza de la lista y continuo con la cola.
                                   (cons (car listaHistorial)(agregarListaHistorial (cdr listaHistorial) (+ i 1) contenido usuario fecha))
                                   )
                               )
  )

; Descripcion: funcion que agrega texto a un documento, para ello es necesario
;              ser propiertatio o tener permiso de escritura. Retorna paradigmadocs
;              con la lista de documento actualizada, con el nuevo contenido del
;              documento. Se utiliza recursion natural en la funcion agregarListaHistorial.
; Dominio: TDA paradigmadocs X entero positivo X TDA fecha X string
; Recorrido: TDA paradigmadocs
(define add(lambda (paradigmadocs)(lambda (idDoc fecha contenidoTexto)
                                    ; Se verifica si hay un usuario conectado.
                                    (if (conectado? (getListaUsuario paradigmadocs))
                                        ; caso verdadero: Se verifica si ese usuario es
                                        ; propietario del archivo o tiene permiso de escritua.
                                        (if (or
                                             ; es propietario?
                                             (equal?
                                              (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1)))
                                              (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                             ; o tiene Permisos?
                                             (tienePermisoEscritura?
                                              (getListaPermiso (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1)))
                                              (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                              )
                                             )
                                            ; caso verdadero: Primero se actuliza la lista
                                            ; de documentos y luego la lista de usuario.
                                            (setListaUsuario
                                             ; Para actualizar la lista de documentos,
                                             (setListaDocumentos
                                              ; se le pasa la actual plataforma
                                              paradigmadocs
                                              ; y actualizo la lista de documentos.
                                              (actualizarListaDocumentos
                                               (getListaDocumentos paradigmadocs)
                                               ; Antes de agregar el contenido, 
                                               (setContenido
                                                ; se actualiza la lista de historial del documento.
                                                ; Para ello, se agrega la version activa al historial.
                                                (setListaHistorial
                                                 ; Se le pasa el documento
                                                 (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))
                                                 ; y la lista de historial actualizada.
                                                 (agregarListaHistorial
                                                  ; se le pasa la lista de historial del documento
                                                  (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                  ; un acumulador para saber el id de la version.
                                                  0
                                                  ; el contenido de la version activa.
                                                  (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                  ; el nombre de quien modifico el contenido del documento.
                                                  (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                  ; la fecha de modificacion
                                                  fecha)
                                                 )
                                                ; ahora el contenido del documento se actualiza, se une el 
                                                ; contenido de la version activa y el texto que se quiere
                                                ; agregar, pasando esta ultima por la funcion de encriptado.
                                                (string-append
                                                 (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                 " "
                                                 ((getFE paradigmadocs) contenidoTexto)
                                                 )
                                                )
                                               )
                                              )
                                             ; luego de actualizar la lista de documentos, se desconecta el usuario.
                                             (agregaLista
                                              (getListaUsuario paradigmadocs)
                                              (setEstado
                                               (buscarConectado(getListaUsuario paradigmadocs))
                                               "Desconectado")
                                              )
                                             )
                                            ; caso falso: si no tiene permiso o no es
                                            ; propietario, se desconecta al usuario.
                                            (setListaUsuario
                                             paradigmadocs
                                             (agregaLista
                                              (getListaUsuario paradigmadocs)
                                              (setEstado
                                               (buscarConectado(getListaUsuario paradigmadocs))
                                               "Desconectado")))
                                            )
                                        ; caso falso: si no hay usuarios conectados,
                                        ; se retorna la plataforma.
                                        paradigmadocs
                                        )
                                    )
             )
  )
; restoreVersion

; Descripcion: funcion que restaura una version x de un documento, solo el 
;              propietario puede hacerlo. Retorna paradigmadocs con la version 
;              del documento restaurada. Se utiliza recursion natural.
; Dominio: TDA paradigmadocs X entero positivo X entero positivo
; Recorrido: TDA paradigmadocs 
(define restoreVersion(lambda (paradigmadocs)(lambda(idDoc idVersion)
                                               ; Se verifica si hay un usuario conectado,
                                               (if (conectado? (getListaUsuario paradigmadocs))
                                                   ; caso verdadero: se verifica si es propietario del documento.
                                                   (if
                                                    ; es propietario?
                                                    (equal?
                                                     (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1)))
                                                     (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                                    ; caso verdadero: Primero se actualiza la lista de documentos
                                                    ; y luego se desconecta al usuario.
                                                    (setListaUsuario
                                                     ; para actualizar la lista,
                                                     (setListaDocumentos
                                                      ; se le pasa la actual plataforma
                                                      paradigmadocs
                                                      ; y agrego el documento actualizado a la lista.
                                                      (actualizarListaDocumentos
                                                       (getListaDocumentos paradigmadocs)
                                                       ; para actualizar el documento
                                                       (setContenido
                                                        ; primero se agrega la version activa a la lista
                                                        ; de historial del documento.
                                                        (setListaHistorial
                                                         (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))
                                                         ; para ello,
                                                         (agregarListaHistorial
                                                          ; se pasa la lista de historial.
                                                          (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                          ; acumulador para el id de la version.
                                                          0
                                                          ; contenido de la version activa.
                                                          (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                          ; nombre del usuario contectado.
                                                          (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                          ; fecha de la modificacion 
                                                          (getFechaModificacion(list-ref(getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) idVersion))
                                                          )
                                                         )
                                                        ; despues le cambio el contenido.
                                                        (getContenidoVersion(list-ref(getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) idVersion))
                                                        )
                                                       )
                                                      )
                                                     ; Ahora se desconecta el usuario.
                                                     (agregaLista
                                                      (getListaUsuario paradigmadocs)
                                                      (setEstado
                                                       (buscarConectado(getListaUsuario paradigmadocs))
                                                       "Desconectado")
                                                      )
                                                     )
                                                    ; caso falso: si no es propietario, se desconecta el usuario.
                                                    (setListaUsuario
                                                     paradigmadocs
                                                     (agregaLista
                                                      (getListaUsuario paradigmadocs)
                                                      (setEstado
                                                       (buscarConectado(getListaUsuario paradigmadocs))
                                                       "Desconectado")
                                                      )
                                                     )
                                                    )
                                                   ; caso falso: si no hay usuarios conectados,
                                                   ; se retorna la plataforma.
                                                   paradigmadocs
                                                   )
                                               )
                        )
  )
; funcion revokeAllAccesses

; Descripcion: funcion que vacia la lista de permisos de un documento si el usuario
;              es propietario de él. Retorna el documento con o sin el cambio.
; Dominio: TDA documento X string
; Recorrido: TDA documento
(define sacarPermisos (lambda (documento usuario)
                        ; Se verifica si es propietario del documento,
                        (if (equal? (getAutor documento) usuario)
                            ; caso verdadero: vacio la lista de permisos del documento.
                            (setListaPermiso documento null)
                            ; caso falso: retorno el documento.
                            documento)
                        )
  )

; Descripcion: funcion que elimina todos los permisos otorgados por un usuario
;              en sus documentos, solo un usuario con documentos puede hacerlo.
;              Retorna paradigmadocs alterado. En esta funcion se utiliza map y filter.
; Dominio: TDA paradigmadocs
; Recorrido: TDA paradigmadocs
(define revokeAllAccesses(lambda (paradigmadocs)
                           ; Se verifica si hay un usuario conectado,
                           (if (conectado? (getListaUsuario paradigmadocs))
                               ; caso verdadero: se verifica si ese usuario
                               ; tiene documentos en la plataforma,
                               (if (not(null? (filter (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))(getListaDocumentos paradigmadocs))))
                                   ; caso verdadero: Primero se actualiza la lista
                                   ; de documentos y luego se desconecta al usuario.
                                   (setListaUsuario
                                    ; para actualizar la lista de documentos,
                                    (setListaDocumentos
                                     ; se le pasa la actual plataforma
                                     paradigmadocs
                                     ; y se aplica la funcion map a la lista
                                     ; de documentos.
                                     (map
                                      ; funcion que se aplica a cada elemento de la lista.
                                      (lambda(documento)
                                        (sacarPermisos
                                         documento
                                         (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                      ; lista a que se aplicara la funcion.
                                      (getListaDocumentos paradigmadocs)
                                      )
                                     )
                                    ; ahora se desconecta el usuario.
                                    (agregaLista
                                     (getListaUsuario paradigmadocs)
                                     (setEstado
                                      (buscarConectado(getListaUsuario paradigmadocs))
                                      "Desconectado")))
                                   ; caso falso: si no tiene documentos, se desconecta
                                   ; el usuario.
                                   (setListaUsuario
                                    paradigmadocs
                                    (agregaLista
                                     (getListaUsuario paradigmadocs)
                                     (setEstado
                                      (buscarConectado(getListaUsuario paradigmadocs))
                                      "Desconectado")
                                     )
                                    )
                                   )
                               ; caso falso: si no hay un usuario contectado,
                               ; se retorna la plataforma.
                               paradigmadocs
                               )
                           )
  )

; search

; Descripcion: funcion que verifica si un usuario tiene permiso de 
;              lectura en un documento. Retorna un booleano.
; Dominio: Lista TDA permiso X string
; Recorrido: Booleano.
(define tienePermisoLectura?(lambda(listaPermiso usuario)
                              ; Se verifica si la lista es vacia,
                              (if (null? listaPermiso)
                                  ; caso verdadero: se retorna un False.
                                     #f
                                     ; caso falso: se verifica si en la cabeza de la
                                     ; lista de permisos, el acceso dado es para el
                                     ; usuario entregado y si el permiso es de lectura.
                                     (if (and(equal? (getUsuario (car listaPermiso)) usuario)
                                             (equal? (getPermiso (car listaPermiso)) #\r))
                                         ; caso verdadero: se retorna un True
                                         #t
                                         ; caso falso: se verifica con la cola de la lista.
                                         (tienePermisoEscritura? (cdr listaPermiso) usuario)
                                         )
                                     )
                              )
  )

; Descripcion: funcion que verifica si en una de las versiones de un
;              documento en una lista de historial se contiene el texto
;              entregado. Retorna un booleano.
; Dominio: Lista TDA historial X string X DecryptFunction
; Recorrido: Booleano.
(define search-substring-historial(lambda(historial substring funciondesencriptado)
                                    ; Se verifica si la lista es vacia,
                                    (if (null? historial)
                                        ; caso verdadero: no se encontro el texto,
                                        ; retorna un False.
                                        #f
                                        ; caso falso: se verifica si el contenido
                                        ; del historial de la cabeza de la lista
                                        ; contiene al texto entregado.
                                        (if (string-contains? (funciondesencriptado (getContenidoVersion(car historial))) substring)
                                            ; caso verdadero: se retorna un True
                                            #t
                                            ; caso falso: se verifica con la cola de la lista.
                                            (search-substring-historial (cdr historial) substring funciondesencriptado)
                                            )
                                        )
                                    )
  )

; Descripcion: funcion que verifica si el usuario es propietario, tiene
;              permiso de lectura o de escritura en el documento, y luego
;              verifica si en las versiones del documento se encuntra el
;              texto entregado. Retorna un booleano.
; Dominio: TDA documento X string X string X DecryptFunction
; Recorrido: Booleano
(define filtroSearch(lambda (documento nombreusuario texto FD)
                      ; Se verifica si el usuario es propietraio, si
                      ; tiene permiso de escritura o de lectura.
                      (if(or
                          ; es propietario?
                          (equal? (getAutor documento) nombreusuario)
                          ; tiene permiso de escritura?
                          (tienePermisoEscritura? (getListaPermiso documento) nombreusuario)
                          ; tiene permiso de lectura?
                          (tienePermisoLectura? (getListaPermiso documento) nombreusuario))
                         ; caso verdadero: se verifica si la version
                         ; activa o anteriores contienen el texto entregado.
                         (or
                          ; en la version activa?
                          (string-contains? (FD (getContenido documento)) texto)
                          ; en las versiones anteriores?
                          (search-substring-historial (getListaHistorial documento) texto FD))
                         ; caso falso: retorno un False
                         #f
                         )
                      )
  )

; Descripcion: funcion que busca todos los documentos que contengan el texto
;              entregado, se revisa tanto la version activa como en la versiones 
;              previas de dichos documentos. Esta busqueda se aplica en los 
;              documentos donde el usuario es propietario, tiene permiso de 
;              lectura o de escritura. Retorna una lista de documentos.
;              Se utiliza la funcion filter.
; Dominio: TDA paradigmadocs X string.
; Recorrido: Lista TDA documentos.
(define search(lambda(paradigmadocs)(lambda (texto)
                                      ; Se verifica si hay un usuario conectado.
                                      (if (conectado? (getListaUsuario paradigmadocs))
                                          ; caso verdadero: filtro la lista de documentos de la plataforma.
                                          (filter
                                           ; funcion de filtro.
                                           (lambda(documento)
                                             (filtroSearch
                                              documento
                                              (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                              texto
                                              (getFD paradigmadocs)))
                                           ; listra filtrada.
                                           (getListaDocumentos paradigmadocs))
                                          ; caso falso: se retorna null.
                                          null)
                                      )
                )
  )
; paradigmadocs->string

; Descripcion: funcion que verifica si una lista esta vacia, si es asi retorna
;              un string, si no pasa la lista a string. 
; Dominio: Lista X funcion X DecryptFunction.
; Recorrido: String.
(define decision(lambda(lista funcion FD)
                  ; Si la lista es vacia,
                  (if (null? lista)
                      ; caso verdadero: no hay datos en la lista.
                      "     No hay informacion en la lista.\n"
                      ; caso falso: paso la lista a string.
                      (funcion lista "" 1 FD)
                      )
                  )
  )

; Descripcion: funcion que pasa un TDA historial a string. Retorna un string.
; Dominio: TDA historial X DecryptFunction.
; Recorrido: string.
(define Historial->string(lambda(historial FD)
                           (string-append
                            "Id version: "
                            (number->string (car historial))
                            ". Contenido: "
                            (FD (getContenidoVersion historial))
                            ". Usuario: "
                            (getUsuarioModificador historial)
                            ". Fecha: "
                            (fecha->string (getFechaModificacion historial))
                            )
                           )
  )

; Descripcion: funcion que pasa una lista de TDA historial a string.
;              Retorna un string. Se utiliza recursion de cola.
; Dominio: Lista TDA historial X string X entero positivo X DecryptFunction.
; Recorrido: String.
(define listaHistorial->string(lambda(listaHistorial string i FD)
                                ; Se verifica si la lista esta vacia,
                                (if (null? listaHistorial)
                                    ; caso verdadero: se retorna un string.
                                    string
                                    ; caso falso: se agrega el acumulador y se agrega
                                    ; el historial de la cabeza de la lista al string,
                                    ; se continua con el acumulador +1 y con la cola de la lista.
                                    (listaHistorial->string (cdr listaHistorial) (string-append string "     " (number->string i) ". " (Historial->string (car listaHistorial) FD) "\n") (+ i 1) FD)
                                    )
                                )
  )

; Descripcion: funcion que pasa el tipo de permiso de un TDA acceso de char
;              a string. Retorna un string.
; Dominio: TDA acceso
; Recorrido: string.
(define tipoPermiso(lambda(acceso)
                     (cond
                       [(equal? (getPermiso acceso) #\w) "Escritura"]
                       [(equal? (getPermiso acceso) #\r) "Lectura"]
                       [(equal? (getPermiso acceso) #\c) "Comentarios"])
                     )
  )

; Descripcion: funcion que pasa un TDA permiso a string. Retorna un string.
; Dominio: TDA acceso
; Recorrido: string.
(define permiso->string(lambda(acceso)
                         (string-append
                          "Usuario: "
                          (getUsuario acceso)
                          ". Permiso: "
                          (tipoPermiso acceso))
                         )
  )

; Descripcion: funcion que pasa una lista de TDA acceso a string.
;              Retorna un string. Se utiliza recursion de cola.
; Dominio: Lista TDA acceso X string X entero positivo X DecryptFunction.
; Recorrido: String 
(define listaPermisos->string(lambda(listaPermiso string i FD)
                               ; Se verifica si la lista esta vacia,
                               (if (null? listaPermiso)
                                   ; caso verdadero: se retorna un string.
                                   string
                                   ; caso falso: Se agrega el acumulador y el
                                   ; permiso de la cabeza de la lista al string,
                                   ; se continua con la cola de la cabeza.
                                   (listaPermisos->string (cdr listaPermiso) (string-append string "     " (number->string i) ". " (permiso->string (car listaPermiso)) "\n") (+ i 1) FD)
                                   )
                               )
  )

; Descripcion: funcion que pasa un TDA documento a string. Retorna un string.
; Dominio: TDA documento X DecryptFunction.
; Recorrido: String
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

; Descripcion: funcion que pasa una lista de TDA documento a string.
;              Retorna un stirng que representa la lista de documentos.
;              Se utliza recursion de cola.
; Dominio: Lista TDA documento X string X entero positivo X DecryptFunction.
; Recorrido: string.
(define listaDocumento->string(lambda(lista string i FD)
                                ; Se verifica si la lista es vacia,
                                (if (null? lista)
                                    ; caso verdadero: retorno el string
                                    string
                                    ; caso falso: se agrega el acumulador, la lista de permiso, la 
                                    ; lista de historial y el documento de la cabeza de la lista
                                    ; al string. Se continua con la cola de la lista y con el acumulador + 1.
                                    (listaDocumento->string (cdr lista) (string-append string "  "(number->string i) ". " (documento->string (car lista) FD) "\n\n     Permisos:\n" (decision (getListaPermiso(car lista)) listaPermisos->string FD) "\n     Historial:\n" (decision (getListaHistorial(car lista)) listaHistorial->string FD) "\n") (+ i 1) FD)
                                    )
                                )
  )

; Descripcion: funcion que pasa un TDA fecha a string.
; Dominio: TDA fecha
; Recorrido: string
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

; Descripcion: funcion que pasa la plataforma paradigmadocs a string
;              para conocer sus datos. Retorna un string.
; Dominio: TDA paradigmadocs
; Recorrido: string
(define paradigmadocs->string(lambda(paradigmadocs)
                               ; Se verifica si hay un usuario conectado:
                               (if (conectado? (getListaUsuario paradigmadocs))
                                   ; caso verdadero: se pasa a string todos los datos del usuario conectado.
                                   (string-append
                                    "DUCKDOCS:\n Bienvenido "
                                    (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                    " (fecha de creacion: "
                                    (fecha->string (getfecha(buscarConectado(getListaUsuario paradigmadocs))))
                                    "). Su lista de documentos es:\n"
                                    (decision (filter (lambda(documento)(equal? (getAutor documento) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))) (getListaDocumentos paradigmadocs)) listaDocumento->string (getFD paradigmadocs))
                                    "\n Los documentos que han sido compartido con su cuenta son:\n"
                                    (decision (filter (lambda(documento)(tienePermiso? (getListaPermiso documento) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))) (getListaDocumentos paradigmadocs)) listaDocumento->string (getFD paradigmadocs))
                                    "\n\n Plataforma creada por Leo Vergara Sepulveda. Fecha de creacion: "
                                    (fecha->string (getFecha paradigmadocs))
                                    ". DuckDocs©."
                                    )
                                   ; caso falso: se pasa a string todos los datos de la plataforma.
                                   (string-append
                                    "DUCKDOCS:\n Los documentos presente en la plataforma DuckDocs son:\n"
                                    (decision (getListaDocumentos paradigmadocs) listaDocumento->string (getFD paradigmadocs))
                                    "\n\n Plataforma creada por Leo Vergara Sepulveda. Fecha de creacion: "
                                    (fecha->string (getFecha paradigmadocs))
                                    ". DuckDocs©."
                                    )
                                   )
                               )
  )

; (opcionales)
; delete

; Descripcion: funcion que entrega la longitud final del substring.
;              Retorna un entero positivo.
; Dominio: String X entero positivo.
; Recorrido: entero positivo.
(define longitud(lambda(contenido numerosdecaracteres)
                  ; Se verifica si la cantidad de caracteres a eliminar
                  ; no supera la longitud del contenido,
                  (if (> (string-length contenido) numerosdecaracteres)
                      ; caso verdadero: se retorna la longitud y n.
                      (- (string-length contenido) numerosdecaracteres)
                      ; caso falso: se retorna un 0.
                      0
                      )
                  )
  )

; Descripcion: funcion que elimina los n ultimos caracteres del contenido
;              de un documento. Retorna paradigmadocs actualizada. Se utiliza
;              recursion natural en actualizarListaDocumentos,
;              agregaLista y agregarListaHistorial.
; Dominio: TDA paradigmadocs X entero positivo X TDA fecha X enterio positivo.
; Recorrido: TDA paradigmadocs.
(define delete(lambda(paradigmadocs)(lambda(idDoc fecha numerodecaracteres)
                                      ; Se verifica si hay un usuario conectado,
                                      (if (conectado? (getListaUsuario paradigmadocs))
                                          ; caso verdadero: se verifica si ese usuario es
                                          ; autor o tiene permiso de escritura en el documento,
                                          (if (or
                                               ; es autor?
                                               (equal? (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                               ; tiene permiso de escritura?
                                               (tienePermisoEscritura? (getListaPermiso (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                              ; caso verdadero: Primero se edita el documento
                                              ; y luego se desconecta al usuario.
                                              (setListaUsuario
                                               ; Se actuliza la lista de documentos,
                                               (setListaDocumentos
                                                ; se pasa la plataforma
                                                paradigmadocs
                                                ; la lista de documentos con el documento actualizado.
                                                (actualizarListaDocumentos
                                                 ; se le pasa la actual lista de documentos
                                                 (getListaDocumentos paradigmadocs)
                                                 ; primero agrego la version activa al historial
                                                 ; del documento y luego cambio el contenido.
                                                 (setContenido
                                                  ; Para agregar la version al historial,
                                                  (setListaHistorial
                                                   ; se le pasa el documento con id = idDoc.
                                                   (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))
                                                   ; se agrega la nueva version a la lista de historial del documento.
                                                   (agregarListaHistorial
                                                    ; se le pasa la lista de historial del documento.
                                                    (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                    ; un acumulador.
                                                    0
                                                    ; el contenido de la version activa
                                                    (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                    ; el nombre del usuario quien hizo la modificacion.
                                                    (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                    ; la fecha de modificacion.
                                                    fecha))
                                                  ; Ahora se cambia el contenido del documento. Se genera
                                                  ; un substring desde el comienzo hasta el final - n.
                                                  (substring
                                                   ; se le pasa el contenido de la version activa,
                                                   (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                   ; el comienzo del string.
                                                   0
                                                   ; el final que es igual a la longitud del contenido - n.
                                                   (longitud (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))) numerodecaracteres)
                                                   )
                                                  )
                                                 )
                                                )
                                               ; Ahora se desconecta al usuario.
                                               (agregaLista
                                                (getListaUsuario paradigmadocs)
                                                (setEstado
                                                 (buscarConectado(getListaUsuario paradigmadocs))
                                                 "Desconectado")
                                                )
                                               )
                                              ; caso falso: si no es autor o no tiene permiso,
                                              ; desconecto al usuario.
                                              (setListaUsuario
                                               paradigmadocs
                                               (agregaLista
                                                (getListaUsuario paradigmadocs)
                                                (setEstado
                                                 (buscarConectado(getListaUsuario paradigmadocs))
                                                 "Desconectado")
                                                )
                                               )
                                              )
                                          ; caso falso: si no hay un usuario contectado,
                                          ; se retorna la plataforma.
                                          paradigmadocs
                                        )
                                      )
                )
  )
; searchAndReplace

; Descripcion: funcion que verifica si un substring esta contenido en un string. Retorna un booleano.
; Dominio: Lista char X lista char X booleano.
; Recorrido: booleano.
(define soniguales(lambda (string substring v1)
                    ; Si el substring esta vacio, 
                    (if (null? substring)
                        ; caso verdadero: se retorna el booleano V1.
                        v1
                        ; caso falso: si el string esta vacio,
                        (if (null? string)
                            ; caso verdadero: no hay mas para comparar,
                            ; se retorna un False.
                            #f
                            ; caso falso: Se verifica si la cabeza del string es igual a la del substring,
                            (if (equal? (car string) (car substring))
                                ; caso verdadero: continuo con la cola del string y del subtring.
                                (soniguales (cdr string) (cdr substring) #t)
                                ; caso falso: se retorna un False.
                                #f)
                            )
                        )
                    )
  )

; Descripcion: funcion que busca las posiciones de un texto buscado en otro texto.
;              Retorna una lista con las posiciones. Se utiliza recursion de cola.
; Dominio: Lista char X Lista char X entero positivo X lista nula.
; Recorrido: Lista enteros positivos.
(define posiciones(lambda(listaString listaSubstring i listaPos)
                    ; Se verifica si la lista del texto esta vacia,
                    (if (null? listaString)
                               ; caso verdadero: se retorna la lista de posiciones invertida.
                               (reverse listaPos)
                               ; caso falso: se verifica si la cabeza de la lista del texto es
                               ; igual a la cabeza de la lista del texto buscado. 
                               (if (equal? (car listaString) (car listaSubstring))
                                   ; caso verdadero: Se verifica si los siguientes char de ambas listas son iguales,
                                   (if (soniguales listaString listaSubstring #t)
                                       ; caso verdadero: se agrega la posicion a la lista y se continua con
                                       ; la cola de la lista del texto y el acumulador +1.
                                       (posiciones (cdr listaString) listaSubstring (+ i 1) (cons i listaPos))
                                       ; caso falso: se continua con la cola de la lista del texto y el acumulador +1.
                                       (posiciones (cdr listaString) listaSubstring (+ i 1) listaPos)
                                       )
                                   ; caso falso: se continua con la cola de la lista del texto y el acumulador +1.
                                   (posiciones (cdr listaString) listaSubstring (+ i 1) listaPos)))
                    )
  )

; Descripcion: funcion que encapsula la funcion posiciones. retorna una lista de enteros.
; Dominio: String X String
; Recorrido: Lista enteros positivos.
(define string-search-all(lambda(string substring)
                           (posiciones
                            ; se pasa el contenido a lista,
                            (string->list string)
                            ; se pasa el texto buscado a lista,
                            (string->list substring)
                            ; un acumulador,
                            0
                            ;y una lista vacia.
                            null)
                           )
  )

; Descripcion: funcion que reemplaza en el contenido de un documento el
;              texto buscado por el texto de reemplazo. Retorna el contenido
;              alterado. Se utiliza recursion natural.
; Dominio: String X string X string X lista enteros positivos X enterio positivo 
; Recorrido: String
(define reemplazar(lambda(contenido replaceText stringAux lisPos i searchText)
                    ; si la lista de posiciones esta vacia,
                      (if (null? lisPos)
                          ; caso verdadero: se copia lo ultimo que queda y se retorna.
                          (string-append stringAux (substring contenido i (string-length contenido)))
                          ; caso falso: se agrega el contenido desde i hasta
                          ; la posicion de la cabeza de la lista de posiciones
                          ; y el texto de reemplazo. Luego se continua con la
                          ; cola de la lista y a lo cambio por la cabeza de la
                          ; lista + la longitud del texto buscado.
                          (reemplazar contenido replaceText (string-append stringAux (substring contenido i (car lisPos)) replaceText) (cdr lisPos) (+ (car lisPos) (string-length searchText)) searchText))
                    ))


; Descripcion: funcion que encapsula la funcion reemplazar.
;              Retorna un string. se utiliza recursion de cola en
;              reemplazar.
; Dominio: string X string X string.
; Recorrido: string.
(define reemplazarEncap(lambda (contenido replaceText searchText)
                         (reemplazar
                          ; se pasa el contenido
                          contenido
                          ; se pasa el texto de reemplazo
                          replaceText
                          ; se pasa el string donde se guardara el cambio.
                          ""
                          ; una lista con las posiciones del texto buscado en el contenido.
                          (string-search-all contenido searchText)
                          ; un acumulador
                          0
                          ; texto de busqueda
                          searchText)
                         )
  )

; Descripcion: funcion que busca un texto seleccionado en el contenido de un
;              documento y lo reemplaza por otro. Retorna paradigmadocs
;              actualizado. Se utiliza recursion de cola y natural. 
; Dominio: TDA paradigmadocs X entero positivo X TDA fecha X string X string.
; Recorrido: TDA paradigmadocs.
(define searchAndReplace(lambda(paradigmadocs)(lambda(idDoc fecha searchText replaceText)
                                                ; Se verifica si hay un usuario conectado,
                                                (if (conectado? (getListaUsuario paradigmadocs))
                                                    ; caso verdadero: se verifica si el usuario
                                                    ; es autor o tiene permiso de escritura,
                                                    (if (or
                                                         ; es autor?
                                                         (equal? (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                                         ; tiene permiso de escritura?
                                                         (tienePermisoEscritura? (getListaPermiso (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                                        ; caso verdadero: primero se actualiza el
                                                        ; documento y luego se desconecta el usuario.
                                                        (setListaUsuario
                                                         ; se cambia la lista de documento de paradigmadocs,
                                                         (setListaDocumentos
                                                          ; se le pasa la plataforma
                                                          paradigmadocs
                                                          ; se actualiza la lista de documentos
                                                          (actualizarListaDocumentos
                                                           ; se pasa la lista de documentos,
                                                           (getListaDocumentos paradigmadocs)
                                                           ; primero se agrega la version activa al historial
                                                           ; del documento y luego se cambia el contenido.
                                                           (setContenido
                                                            ; se cambia el historial del documento con id = idDoc
                                                            (setListaHistorial
                                                             ; se pasa el documento
                                                             (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))
                                                             ; se agrega la version activa a la lista historial.
                                                             (agregarListaHistorial
                                                              ; se le pasa la lista historial actual del documento,
                                                              (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                              ; un acumulador,
                                                              0
                                                              ; el contenido de la version activa,
                                                              (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                              ; el nombre del usuario que hizo el cambio,
                                                              (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                              ; y la fecha de modificacion.
                                                              fecha)
                                                             )
                                                            ; ahora se reemplaza cada texto buscado por el
                                                            ; texto de reemplazo en el contenido del documento.
                                                            ((getFE paradigmadocs)
                                                             (reemplazarEncap
                                                              ((getFD paradigmadocs)(getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))))
                                                              replaceText
                                                              searchText))
                                                            )
                                                           )
                                                          )
                                                         ; ahora se desconecta al usuario.
                                                         (agregaLista
                                                          (getListaUsuario paradigmadocs)
                                                          (setEstado
                                                           (buscarConectado(getListaUsuario paradigmadocs))
                                                           "Desconectado")))
                                                        ; caso falso: si no es autor o si no tiene permiso de
                                                        ; escritura, se desconecta al usuario.
                                                        (setListaUsuario
                                                         paradigmadocs
                                                         (agregaLista
                                                          (getListaUsuario paradigmadocs)
                                                          (setEstado
                                                           (buscarConectado(getListaUsuario paradigmadocs))
                                                           "Desconectado")))
                                                        )
                                                    ; caso falso: si no hay un usuario conectado,
                                                    ; se retorna paradigmadocs.
                                                    paradigmadocs
                                                    )
                                                )
                          )
  )

; applyStyles

; Descripcion: funcion que un char a string con el mismo formato. Retorna un string.
; Dominio: char
; Recorrido: string.
(define generar(lambda(estilo)
                 (list->string (list #\# #\\ estilo))
                 )
  )

; Descripcion: funcion que aplica los estilos a un texto. Retorna un string.
;              Se utiliza recursion de cola.
; Dominio: String X char X ... X char
; Recorrido: String
(define aplicar(lambda(searchText styles)
                 ; Si verifica si la lista de estilos esta vacia,
                 (if (null? styles)
                     ; caso verdadero: se retorna el string con los estilos aplicados.
                     searchText
                     ; caso falso: se agrega "estilo - texto buscado - estilo" a texto
                     ; buscado y se continua con la cola de la lista.
                     (aplicar (string-append (generar (car styles)) " " searchText " " (generar (car styles))) (cdr styles))
                     )
                 )
  )

; Descripcion: funcion que aplica un estilo de texto a un texto buscado en el
;              contenido de un documento. Retorna paradigmadocs. Se utiliza
;              recursion de cola y natural. 
; Dominio: TDA paradigmadocs X entero positivo X TDA fecha X string X char X ... X char
; Recorrido: TDA paradigmadocs
(define applyStyles(lambda(paradigmadocs)(lambda(idDoc fecha searchText . styles)
                                           ; Se verifica si hay un usuario conectado.
                                           (if (conectado? (getListaUsuario paradigmadocs))
                                               ; caso verdadero: se verifica si el usuario es autor o tiene permiso de escritura,
                                               (if (or
                                                    ; es autor? 
                                                    (equal? (getAutor(list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs))))
                                                    ; o tiene permiso de escritura?
                                                    (tienePermisoEscritura? (getListaPermiso (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))) (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))))
                                                   ; caso verdadero: primero se aplican los estilos y luego se desconecta al usuario.
                                                   (setListaUsuario
                                                    ; se cambia la lista de documentos.
                                                    (setListaDocumentos
                                                     ; se pasa la plataforma
                                                     paradigmadocs
                                                     ; se actualiza la lista de documentos
                                                     (actualizarListaDocumentos
                                                      ; se le pasa el la lista de documentos
                                                      (getListaDocumentos paradigmadocs)
                                                      ; Primero se agrega la version activa a la lista de historial
                                                      ; y se cambia el contenido del documento id = idDoc.
                                                      (setContenido
                                                       ; se cambia la lista de historial.
                                                       (setListaHistorial
                                                        ; se le pasa el documento
                                                        (list-ref(getListaDocumentos paradigmadocs) (- idDoc 1))
                                                        ; se agrega la version a la lista de historial.
                                                        (agregarListaHistorial
                                                         ; se le pasa la lista de historial del documento
                                                         (getListaHistorial(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                         ; un acumulador
                                                         0
                                                         ; el contenido de la version activa
                                                         (getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1)))
                                                         ; el nombre del usuario quien hizo el cambio.
                                                         (getNombreusuario(buscarConectado(getListaUsuario paradigmadocs)))
                                                         ; la fecha de modificacion.
                                                         fecha)
                                                        )
                                                       ; ahora se aplican los estilos.
                                                       ((getFE paradigmadocs)
                                                        ; se reemplaza el contenido del texto con el texto
                                                        ; buscado con los estilos aplicados.
                                                        (reemplazarEncap
                                                         ((getFD paradigmadocs)(getContenido(list-ref(getListaDocumentos paradigmadocs)(- idDoc 1))))
                                                         (aplicar searchText styles)
                                                         searchText))
                                                       )
                                                      )
                                                     )
                                                    ; ahora se desconecta al usuario.
                                                    (agregaLista
                                                     (getListaUsuario paradigmadocs)
                                                     (setEstado
                                                      (buscarConectado(getListaUsuario paradigmadocs))
                                                      "Desconectado")))
                                                   ; caso falso: si no es autor o tiene permiso,
                                                   ; se desconecta al usuario.
                                                   (setListaUsuario
                                                    paradigmadocs
                                                    (agregaLista
                                                     (getListaUsuario paradigmadocs)
                                                     (setEstado
                                                      (buscarConectado(getListaUsuario paradigmadocs))
                                                      "Desconectado")))
                                                   )
                                               ; caso falso: si no hay un usuario conectado se retorna la plataforma.
                                               paradigmadocs
                                               )
                                           )
                     )
  )

; comment
; ctrlZ y ctrlY

; funcion encriptado y desencriptado.

; Descripcion: funcion que cambia las vocales por letras del alfabeto griego. Retorna una lista de Char.
; Dominio: Lista char
; Recorrido: Lista char
(define cambiarLetras(lambda (lista)
                       ; Si la lista esta vacia,
                       (if (null? lista)
                           ; caso verdadero: se retorna un null.
                           null
                           ; caso falso: se verifica si la cabeza de la lista es una vocal
                           ; o letra griega y se cambia de vocal a letra o viceversa.
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

; Descripcion: funcion que encripta el contenido de un documento, cambiando
;              las vocales a letras del alfabeto griego. Retorna un string. 
; Dominio: String
; Recorrido: String
(define encryptFunction (lambda (contenido)
                          (list->string(cambiarLetras (string->list contenido)))
                          )
  )

; Descripcion: funcion que desencripta el contenido de un documento, cambiando
;              las letras del alfabeto griego a las vocales. Retorna un string. 
; Dominio: String
; Recorrido: String
(define DecryptFunction (lambda (contenido)
                          (list->string(cambiarLetras (string->list contenido)))
                          )
  )

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; Test                                                                                                                       :
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; Se crea la plataforma.
(define DuckDocsVacio(paradigmadocs "DuckDocs" (fecha 1 11 2021) encryptFunction DecryptFunction))

; Register
(define gDocs1 (register DuckDocsVacio (fecha 25 10 2021) "user1" "pass1")) ; se registra usuario 1
(define gDocs2 (register gDocs1 (fecha 25 10 2021) "user2" "pass2")) ; se registra usuario 1
(define gDocs3 (register gDocs2 (fecha 25 10 2021) "user3" "pass3")) ; se registra usuario 1
(define gDocs4 (register gDocs3 (fecha 25 10 2021) "user1" "pass4")) ; no se registra al usuario, nombre igual al usuario 1

; Login, se utilizara la funcion create pero no se dejara registro en la plataforma.
(define gDocs5 ((login gDocs4 "user1" "pass1" create) (fecha 30 08 2021) "doc0" "contenido doc0")) ; usuario 1 se logea y crea doc0
(define gDocs6 ((login gDocs4 "user2" "pass2" create) (fecha 30 08 2021) "doc0" "contenido doc0")) ; usuario 2 se logea y crea doc0
(define gDocs7 ((login gDocs4 "user3" "pass3" create) (fecha 30 08 2021) "doc0" "contenido doc0")) ; usuario 3 se logea y crea doc0

; Create 
(define gDocs8 ((login gDocs4 "user1" "pass1" create) (fecha 30 08 2021) "doc3" "contenido doc3")) ; usuario 1 crea doc3
(define gDocs9 ((login gDocs8 "user2" "pass2" create) (fecha 30 08 2021) "doc0" "contenido doc0")) ; usuario 2 crea doc0
(define gDocs10 ((login gDocs9 "user2" "pass2" create) (fecha 30 08 2021) "doc1" "contenido doc1")) ; usuario 2 crea doc1
(define gDocs11 ((login gDocs10 "user1" "pass1" create) (fecha 30 08 2021) "doc0" "contenido doc0")) ; usuario 1 crea doc0
(define gDocs12 ((login gDocs11 "user2" "pass2" create) (fecha 30 08 2021) "doc5" "contenido doc5")) ; usuario 2 crea doc5
(define gDocs13 ((login gDocs12 "user3" "pass3" create) (fecha 30 08 2021) "doc1" "contenido doc1")) ; usuario 3 crea doc1
(define gDocs14 ((login gDocs13 "user2" "pass2" create) (fecha 30 08 2021) "doc10" "contenido doc10")) ; usuario 2 crea doc10
(define gDocs15 ((login gDocs14 "user3" "pass3" create) (fecha 30 08 2021) "doc2" "contenido doc2")) ; usuario 3 crea doc2

; Share
(define gDocs16 ((login gDocs15 "user2" "pass2" share) 4 (acceso "user1" #\w) (acceso "user3" #\w))) ; usuario 2 comparte su cuarto documento con usuario 1 y 3 con permiso de escritura.
(define gDocs17 ((login gDocs16 "user2" "pass2" share) 2 (acceso "user1" #\c))) ; usuario 2 comparte su segundo documento con usuario 1 para que comente.
(define gDocs18 ((login gDocs17 "user1" "pass1" share) 10 (acceso "user2" #\c))) ; usuario 1 comparte un archivo que no tiene, no se registra en la plataforma.
(define gDocs19 ((login gDocs18 "user3" "pass3" share) 1 (acceso "user1" #\r) (acceso "user2" #\r))) ; usuario 3 comparte su primer documento con usuario 1 y usuario 2 con permiso de lectura.
(define gDocs20 ((login gDocs19 "user3" "pass3" share) 1 (acceso "user1" #\c))) ; usuario 3 cambia el permiso que le dio al usuario 1 en su primer documento.

; Add
(define gDocs21 ((login gDocs20 "user1" "pass1" add) 4 (fecha 8 11 2021) "mas contenido para el texto")) ; usuario 1 agrega mas contenido a un documento de su propiedad.
(define gDocs22 ((login gDocs21 "user1" "pass1" add) 5 (fecha 8 11 2021) "mas contenido para el texto")) ; usuario 1 agrega mas contenido a un documento donde no tiene permiso y no es propietario, no se registra en la plataforma.
(define gDocs23 ((login gDocs22 "user3" "pass3" add) 7 (fecha 8 11 2021) "mas contenido para el texto")) ; usuario 3 agrega mas contenido a un documento donde tiene permiso de escritura.
(define gDocs24 ((login gDocs23 "user2" "pass2" add) 7 (fecha 8 11 2021) "mas contenido para el texto")) ; usuario 2 agrega mas contenido a un documento de su propiedad.

; RestoreVersion, solo el propietario puede hacerlo.
(define gDocs25 ((login gDocs24 "user1" "pass1" restoreVersion) 4 0)) ; usuario 1 restaura la version original del documento.
(define gDocs26 ((login gDocs25 "user2" "pass2" restoreVersion) 7 1)) ; usuario 2 restaura la version 1 del documento.
(define gDocs27 ((login gDocs26 "user3" "pass3" restoreVersion) 7 0)) ; usuario 3 intenta restaurar la version original del documento del usuario 2, no se registra en la plataforma

; revokeAllAccesses, no se dejara registro en la plataforma para probar las siguientes funciones.
(define gDocs28 (login gDocs27 "user2" "pass2" revokeAllAccesses)) ; usuario 2 revoca todos los accesos a sus documentos 
(define gDocs29 (login gDocs27 "user1" "pass1" revokeAllAccesses)) ; usuario 1 revoca todos los accesos a sus documentos
(define gDocs30 (login gDocs27 "user3" "pass3" revokeAllAccesses)) ; usuario 3 revoca todos los accesos a sus documentos

; search
(define gDocs31 ((login gDocs27 "user1" "pass1" search) "contenido")) ; el usuario 1 busca en sus documentos o los compartidos la palabra contenido
(define gDocs32 ((login gDocs27 "user2" "pass2" search) "contenido")) ; el usuario 2 busca en sus documentos o los compartidos la palabra contenido
(define gDocs33 ((login gDocs27 "user3" "pass3" search) "contenido")) ; el usuario 3 busca en sus documentos o los compartidos la palabra contenido
(define gDocs34 ((login gDocs27 "user3" "pass3" search) "doc1")) ; el usuario 3 busca en sus documentos o los compartidos la palabra doc1

; paradigmadocs->string
(define gDocs35 (login gDocs27 "user1" "pass1" paradigmadocs->string)) ; el usuario 1 quiere ver su informacion en la plataforma
(define gDocs36 (login gDocs27 "user2" "pass2" paradigmadocs->string)) ; el usuario 2 quiere ver su informacion en la plataforma
(define gDocs37 (login gDocs27 "user3" "pass3" paradigmadocs->string)) ; el usuario 3 quiere ver su informacion en la plataforma
(define gDocs38 (paradigmadocs->string gDocs27)) ; Se ve la informacion en la plataforma en general.

; delete
(define gDocs39 ((login gDocs27 "user1" "pass1" delete) 1 (fecha 9 11 2021) 3))
(define gDocs40 ((login gDocs39 "user2" "pass2" delete) 3 (fecha 10 11 2021) 4))
(define gDocs41 ((login gDocs40 "user3" "pass3" delete) 8 (fecha 10 11 2021) 10))

; searchAndReplace
(define gDocs42 ((login gDocs41 "user1" "pass1" searchAndReplace) 4 (fecha 10 11 2021) "contenido" "tema"))
(define gDocs43 ((login gDocs42 "user2" "pass2" searchAndReplace) 2 (fecha 10 11 2021) "contenido" "tema"))
(define gDocs44 ((login gDocs43 "user3" "pass3" searchAndReplace) 7 (fecha 10 11 2021) "contenido" "tema"))

; applyStyles
(define gDocs45 ((login gDocs44 "user1" "pass1" applyStyles) 4 (fecha 10 11 2021) "tema" #\t #\n))
(define gDocs46 ((login gDocs45 "user2" "pass2" applyStyles) 2 (fecha 10 11 2021) "tema" #\t #\n))
(define gDocs47 ((login gDocs46 "user3" "pass3" applyStyles) 7 (fecha 10 11 2021) "tema" #\t #\n))

; EncryptFuction y DecryptFunction
(define encriptado1 (encryptFunction "contenido de un documento"))
(define encriptado2 (encryptFunction "cambio las vocales por letras griegas"))
(define encriptado3 (encryptFunction "mas contenido de un documento"))

(define desencriptado1 (DecryptFunction "cξntωnζdξ dω ρn dξcρmωntξ"))
(define desencriptado2 (DecryptFunction "cψmbζξ lψs vξcψlωs pξr lωtrψs grζωgψs"))
(define desencriptado3 (DecryptFunction "mψs cξntωnζdξ dω ρn dξcρmωntξ"))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
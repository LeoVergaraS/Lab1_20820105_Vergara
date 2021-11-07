#lang racket
(provide (all-defined-out))
(require "TDAfecha.rkt")

; contructor TDA documento
(define documento(lambda (autor fecha nombredocumento contenido)
                   (if (and (string? autor)
                            (fecha? fecha)
                            (string? nombredocumento)
                            (string? contenido)
                            )
                       ; caso verdadero
                       (list autor fecha nombredocumento contenido (list))
                       ; caso falso
                       null
                       )
                   )
  )

; pertenencia
(define documento?(lambda (documento)
                    (and (string? (car documento))
                            (fecha? (cadr documento))
                            (string? (caddr documento))
                            (string? (cadddr documento))
                            )
                    )
  )

; selector
(define getAutor(lambda (documento)
                        (car documento)
                        )
  )

(define getFechaCreacion(lambda (documento)
                             (cadr documento)
                             )
  )

(define getNombreDocumento(lambda (documento)
                             (caddr documento)
                             )
  )

(define getContenido (lambda (documento)
                       (cadddr documento)
                       )
  )
(define getListaPermiso (lambda (documento)
                       (cadddr(cdr documento))
                       )
  )

; modificadores
(define setNombreDocumento(lambda (documento nombredocumento)
                            (list (getAutor documento)
                                       (getFechaCreacion documento)
                                        nombredocumento
                                       (getContenido documento)
                                       (getListaPermiso documento)
                                       )
                            )
  )

(define setListaPermiso(lambda (documento listaPermisos)
                            (list (getAutor documento)
                                       (getFechaCreacion documento)
                                       (getNombreDocumento documento)
                                       (getContenido documento)
                                       listaPermisos
                                       )
                            )
  )

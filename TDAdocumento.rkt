#lang racket
(provide (all-defined-out))

; contructor TDA documento
(define documento(lambda (id nombredocumento contenido)
                   (if (and (integer? id)
                            (string? nombredocumento)
                            (string? contenido)
                            )
                       ; caso verdadero
                       (list id nombredocumento contenido)
                       ; caso falso
                       null
                       )
                   )
  )

; pertenencia
(define documento?(lambda (documento)
                    (and (integer? (car documento))
                         (string? (cadr documento))
                         (string? (caddr documento))
                         )
                    )
  )

; selector
(define getiddocumento(lambda (documento)
                        (car documento)
                        )
  )

(define getnombredocumento (lambda (documento)
                             (cadr documento)
                             )
  )

(define getcontenido (lambda (documento)
                       (caddr documento)
                       )
  )

; modificadores
(define setnombredocumento(lambda (documento nombredocumento)
                            (documento (getiddocumento documento)
                                        nombredocumento
                                       (getcontenido documento)
                                       )
                            )
  )

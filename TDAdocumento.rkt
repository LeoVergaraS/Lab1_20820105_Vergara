#lang racket
(provide (all-defined-out))

; contructor TDA documento
(define documento(lambda (nombredocumento contenido)
                   (if (and (string? nombredocumento)
                            (string? contenido)
                            )
                       ; caso verdadero
                       (list nombredocumento contenido)
                       ; caso falso
                       null
                       )
                   )
  )

; pertenencia
(define documento?(lambda (documento)
                    (and (string? (car documento))
                         (string? (cadr documento))
                         )
                    )
  )

; selector
(define getnombredocumento (lambda (documento)
                             (car documento)
                             )
  )

(define getcontenido (lambda (documento)
                       (cadr documento)
                       )
  )

; modificadores
(define setnombredocumento(lambda (documento nombredocumento)
                            (documento  nombredocumento
                                       (getcontenido documento)
                                       )
                            )
  )
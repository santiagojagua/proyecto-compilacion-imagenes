;;; src/handlers/basic.lisp - Handlers básicos para la API
(in-package :mi-api)

;;; GET / - Devuelve "hola lisp"
(define-easy-handler (hola-lisp :uri "/") ()
  (setf (content-type*) "text/plain; charset=utf-8")
  "hola lisp")

;;; GET /api/saludo - Saludo en JSON
(define-easy-handler (api-saludo :uri "/api/saludo") ()
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string 
   `((:mensaje . "hola lisp")
     (:timestamp . ,(get-universal-time))
     (:servidor . "Common Lisp")
     (:endpoint . "/api/saludo"))))

;;; GET /api/saludo/:nombre - Saludo personalizado
(define-easy-handler (saludo-personalizado :uri "/api/saludo/:nombre") (nombre)
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string 
   `((:mensaje . ,(format nil "hola ~a desde lisp" (or nombre "desconocido")))
     (:saludo-personalizado . t)
     (:timestamp . ,(get-universal-time))
     (:parametro-recibido . ,nombre))))

;;; GET /health - Health check del servidor
(define-easy-handler (health-check :uri "/health") ()
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string 
   `((:status . "ok")
     (:servidor . "Hunchentoot")
     (:lenguaje . "Common Lisp")
     (:timestamp . ,(get-universal-time))
     (:puerto . ,(if *acceptor* (acceptor-port *acceptor*) "no-iniciado")))))
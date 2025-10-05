;; test-load.lisp - Verificar carga básica
(format t "Iniciando prueba de carga...~%")

;; Cargar Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Cargar dependencias
(format t "Cargando dependencias...~%")
(ql:quickload '(:hunchentoot :cl-json :alexandria :bordeaux-threads))

;; Intentar cargar main.lisp
(format t "Intentando cargar main.lisp...~%")
(load "src/main.lisp")

(format t "Verificando paquete MI-API...~%")
(if (find-package :mi-api)
    (format t "✅ Paquete MI-API encontrado~%")
    (format t "❌ Paquete MI-API NO encontrado~%"))

(format t "Prueba completada~%")
;;; cargar.lisp - Script para cargar el proyecto rápidamente

;; Cargar Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Cargar dependencias
(format t "📦 Cargando dependencias...~%")
(ql:quickload '(:hunchentoot :cl-json :alexandria))

;; Cargar el archivo principal primero (esto define el paquete MI-API)
(format t "🚀 Cargando mi-api...~%")
(load "src/main.lisp")

;; Ahora podemos usar el paquete MI-API
(in-package :mi-api)

(format t "~%~%")
(format t "✨ ============================================== ✨~%")
(format t "✨            API LISP CARGADA CORRECTAMENTE      ✨~%")
(format t "✨ ============================================== ✨~%~%")

(format t "Comandos disponibles:~%")
(format t "  (mi-api:start-server)     - Iniciar servidor en puerto 8080~%")
(format t "  (mi-api:start-server :port 9090) - Iniciar en puerto personalizado~%")
(format t "  (mi-api:stop-server)      - Detener servidor~%")
(format t "  (mi-api:main)             - Ejecutar función principal~%~%")

(format t "Endpoints disponibles:~%")
(format t "  http://localhost:8080/~%")
(format t "  http://localhost:8080/api/saludo~%")
(format t "  http://localhost:8080/api/saludo/tu-nombre~%")
(format t "  http://localhost:8080/health~%~%")

;; Preguntar si iniciar automáticamente
(format t "¿Iniciar servidor ahora? (s/n): ")
(force-output)
(let ((response (read-line)))
  (when (or (string-equal response "s") 
            (string-equal response "si")
            (string-equal response "y")
            (string-equal response "yes"))
    (main)))  ; Usamos 'main' directamente ya que estamos en el paquete mi-api
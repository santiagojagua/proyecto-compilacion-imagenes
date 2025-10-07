;;; cargar.lisp - Script para cargar el proyecto

;; Cargar Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Cargar dependencias
(format t "📦 Cargando dependencias...~%")
(ql:quickload '(:hunchentoot :cl-json :alexandria :bordeaux-threads
               :opticl :cl-base64 :flexi-streams))

;; Cargar el archivo principal
(format t "🚀 Cargando mi-api...~%")
(load "src/main.lisp")

;; Cambiar al paquete MI-API
(in-package :mi-api)

(format t "~%")
(format t "✨ API LISP CARGADA CORRECTAMENTE ✨~%~%")

(format t "Comandos disponibles:~%")
(format t "  (mi-api:start-server)     - Iniciar servidor en puerto 8080~%")
(format t "  (mi-api:start-server :port 9090) - Iniciar en puerto personalizado~%")
(format t "  (mi-api:stop-server)      - Detener servidor~%")
(format t "  (mi-api:main)             - Ejecutar función principal~%~%")

(format t "Endpoints disponibles:~%")
(format t "  GET  http://localhost:8080/              - 'hola lisp'~%")
(format t "  GET  http://localhost:8080/health        - Health check~%")
(format t "  POST http://localhost:8080/api/rpc       - JSON-RPC Endpoint~%~%")

(format t "Métodos JSON-RPC implementados:~%")
(format t "  procesarLoteParalelo    - Procesa lote de operaciones~%")
(format t "  obtenerProgreso         - Obtiene progreso actual~%")
(format t "  cancelarProcesamiento   - Cancela procesamiento~%")
(format t "  obtenerEstadisticas     - Obtiene estadísticas~%~%")

;; Preguntar si iniciar automáticamente
(format t "¿Iniciar servidor ahora? (s/n): ")
(force-output)
(let ((response (read-line)))
  (when (or (string-equal response "s") 
            (string-equal response "si")
            (string-equal response "y")
            (string-equal response "yes"))
    (main)))
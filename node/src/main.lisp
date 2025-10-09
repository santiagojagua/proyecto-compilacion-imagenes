;;; src/main.lisp - Servidor API Hunchentoot básico
(in-package :mi-api)

;;; ==================== CONFIGURACIÓN ====================
(defvar *acceptor* nil "La instancia del servidor Hunchentoot.")
(defparameter *default-port* 8080 "El puerto por defecto para el servidor.")

;; La instancia global del procesador se crea aquí.
;; Los archivos que la usan (handlers/json-rpc) son cargados después por ASDF.
(defvar job-manager (make-instance 'imgx-job-manager))


;;; ==================== FUNCIONES PRINCIPALES ====================

(defun stop-server ()
  "Detiene el servidor Hunchentoot si está corriendo."
  (when (and *acceptor* (started-p *acceptor*))
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "🛑 Servidor detenido~%")))

(defun start-server (&key (port *default-port*))
  "Inicia (o reinicia) el servidor Hunchentoot."
  (stop-server) ; Asegurarse de que no hay un servidor previo
  (setf *acceptor* (make-instance 'easy-acceptor :port port :address "0.0.0.0"))
  (start *acceptor*)
  (setf hunchentoot:*show-lisp-errors-p* t
        hunchentoot:*show-lisp-backtraces-p* t)
  (format t "~%")
  (format t "🚀 Servidor iniciado en http://localhost:~D~%" port)
  (format t "📋 Endpoints:~%")
  (format t "   GET  /              - hola lisp~%")
  (format t "   GET  /health        - health check~%")
  (format t "   POST /api/rpc       - JSON-RPC~%")
  (format t "~%")
  *acceptor*)

(defun main (&optional (port *default-port*))
  "Función principal para ejecutar el servidor de forma interactiva."
  (format t "~%=== INICIANDO API LISP ===~%~%")
  (start-server :port port)
  (handler-case
      (loop (sleep 10))
    (sb-sys:interactive-interrupt ()
      (format t "~%⏹️  Deteniendo servidor...~%")
      (stop-server)
      (sb-ext:exit))))

;; La ejecución automática se puede manejar desde el script de carga o despliegue
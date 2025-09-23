;;; main.lisp - Servidor API Hunchentoot básico
(ql:quickload '(:hunchentoot :cl-json :alexandria))

(defpackage :mi-api
  (:use :cl :hunchentoot)
  (:export :start-server :stop-server :main))
(in-package :mi-api)

;;; ==================== CONFIGURACIÓN ====================
(defvar *acceptor* nil
  "Instancia del servidor Hunchentoot")

(defparameter *default-port* 8080
  "Puerto por defecto del servidor")

;;; ==================== CARGA DE HANDLERS ====================

;; Cargar los handlers desde el archivo separado
(load "src/handlers/basic.lisp")

;;; ==================== FUNCIONES PRINCIPALES ====================

(defun start-server (&key (port *default-port*))
  "Inicia el servidor Hunchentoot en el puerto especificado"
  ;; Detener servidor anterior si existe
  (when (and *acceptor* (hunchentoot:started-p *acceptor*))
    (stop-server))
  
  ;; Crear e iniciar nuevo servidor
  (setf *acceptor* 
        (make-instance 'easy-acceptor 
                       :port port
                       :address "0.0.0.0"))  ; Escuchar en todas las interfaces
  
  (start *acceptor*)
  
  ;; Mostrar información
  (format t "~%")
  (format t "╔══════════════════════════════════════════════════╗~%")
  (format t "║                🚀 SERVIDOR INICIADO              ║~%")
  (format t "╠══════════════════════════════════════════════════╣~%")
  (format t "║ Servidor: Hunchentoot                            ║~%")
  (format t "║ Lenguaje: Common Lisp                            ║~%")
  (format t "║ Puerto: ~D                                        ║~%" port)
  (format t "║ URL: http://localhost:~D                         ║~%" port)
  (format t "╚══════════════════════════════════════════════════╝~%")
  (format t "~%📋 Endpoints disponibles:~%")
  (format t "   🌐 GET /                      - 'hola lisp'~%")
  (format t "   📊 GET /api/saludo            - Saludo JSON~%")
  (format t "   👋 GET /api/saludo/:nombre    - Saludo personalizado~%")
  (format t "   ❤️  GET /health               - Health check~%")
  (format t "~%Presiona Ctrl+C para detener el servidor~%~%")
  
  *acceptor*)

(defun stop-server ()
  "Detiene el servidor Hunchentoot"
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "~%🛑 Servidor detenido correctamente~%")))

;;; ==================== MANEJO DE SEÑALES SIMPLIFICADO ====================

(defun setup-signal-handler ()
  "Configura manejo básico de señales"
  (format t "🔧 Configurando manejo de interrupciones...~%"))

;;; ==================== FUNCIÓN PRINCIPAL ====================

(defun main (&optional (port *default-port*))
  "Función principal para ejecutar el servidor"
  (format t "~%~%=== INICIANDO API LISP ===~%~%")
  
  (setup-signal-handler)
  (start-server :port port)
  
  ;; Mantener el servidor corriendo
  (handler-case
      (loop 
        (sleep 10)
        (format t "Servidor activo en puerto ~D (~A)~%" 
                port (get-universal-time)))
    (#+sbcl sb-sys:interactive-interrupt 
     #-sbcl simple-error ()
      (format t "~%⏹️  Interrupción recibida, deteniendo servidor...~%")
      (stop-server)
      #+sbcl (sb-ext:exit))))

;;; ==================== EJECUCIÓN AUTOMÁTICA ====================

#-swank
(unless (member "--no-auto-start" (uiop:command-line-arguments) :test #'string=)
  (format t "⏳ Iniciando servidor automáticamente...~%")
  (main))
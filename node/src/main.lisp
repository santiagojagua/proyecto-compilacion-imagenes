;;; main.lisp - Servidor API Hunchentoot básico

;; Cargar dependencias primero
(ql:quickload '(:hunchentoot :cl-json :alexandria :bordeaux-threads :cl-base64 :opticl))

;; Definir el paquete
(defpackage :mi-api
  (:use :cl :hunchentoot)
  (:export :start-server :stop-server :main
           :imgx-job-manager
           :procesar-lote-imgx
           :obtener-progreso
           :cancelar-procesamiento
           :obtener-estadisticas))

(in-package :mi-api)

;;; ==================== CONFIGURACIÓN ====================
(defvar *acceptor* nil)
(defparameter *default-port* 8080)

;; 1) Cargar interfaces/implementación del procesador
(load "src/parallelimageprocessor/imgx.lisp")
(load "src/parallelimageprocessor/imgx-batch.lisp")
(load "src/parallelimageprocessor/interface.lisp")
(load "src/parallelimageprocessor/implementation-imgx.lisp")

;; 2) Instancia global del procesador
(defvar job-manager (make-instance 'imgx-job-manager))

;; 3) Cargar handlers
(load "src/handlers/basic.lisp")
(load "src/handlers/json-rpc.lisp")

;;; ==================== FUNCIONES PRINCIPALES ====================

(defun stop-server ()
  (when *acceptor*
    (stop *acceptor*)
    (setf *acceptor* nil)
    (format t "🛑 Servidor detenido~%")))

(defun start-server (&key (port *default-port*))
  (when (and *acceptor* (started-p *acceptor*))
    (stop-server))
  (setf *acceptor* (make-instance 'easy-acceptor :port port :address "0.0.0.0"))
  (start *acceptor*)
  (format t "~%")
  (format t "🚀 Servidor iniciado en puerto ~D~%" port)
  (format t "📋 Endpoints:~%")
  (format t "   GET  /              - hola lisp~%")
  (format t "   GET  /health        - health check~%")
  (format t "   POST /api/rpc       - JSON-RPC~%")
  (format t "~%")
  *acceptor*)

(defun main (&optional (port *default-port*))
  (format t "~%=== INICIANDO API LISP ===~%~%")
  (start-server :port port)
  
  (handler-case
      (loop (sleep 10))
    (sb-sys:interactive-interrupt ()
      (format t "~%⏹️  Deteniendo servidor...~%")
      (stop-server)
      (sb-ext:exit))))

;; Comentamos la ejecución automática para control manual
;; #-swank
;; (unless (member "--no-auto-start" (uiop:command-line-arguments) :test #'string=)
;;   (main))
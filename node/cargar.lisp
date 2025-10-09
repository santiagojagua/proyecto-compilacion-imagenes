;;; cargar.lisp - Script para cargar el proyecto

;; Cargar Quicklisp si no está ya cargado
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Determinar el directorio del script
(defparameter *project-dir*
  (make-pathname 
   :name nil 
   :type nil 
   :defaults (or *load-truename* 
                 *compile-file-truename* 
                 (truename "."))))

(format t "📂 Directorio del proyecto: ~A~%" *project-dir*)

;; Registrar el directorio del proyecto en ASDF
(pushnew *project-dir* asdf:*central-registry* :test #'equal)

;; Verificar que el archivo .asd existe
(let ((asd-file (merge-pathnames "mi-api.asd" *project-dir*)))
  (if (probe-file asd-file)
      (format t "✅ Encontrado: ~A~%" asd-file)
      (progn
        (format t "❌ ERROR: No se encuentra ~A~%" asd-file)
        (format t "   Archivos en el directorio:~%")
        (dolist (file (directory (merge-pathnames "*.*" *project-dir*)))
          (format t "   - ~A~%" (file-namestring file)))
        (sb-ext:exit :code 1))))

;; Cargar el sistema usando ASDF
(format t "~%📦 Cargando el sistema :mi-api vía ASDF...~%")
(handler-case
    (asdf:load-system :mi-api)
  (error (e)
    (format t "❌ ERROR al cargar el sistema: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Cambiar al paquete para conveniencia del usuario
(in-package :mi-api)

(format t "~%✨ API LISP CARGADA CORRECTAMENTE ✨~%~%")
(format t "Comandos disponibles:~%")
(format t "  (mi-api:start-server)           - Iniciar servidor en puerto 8080~%")
(format t "  (mi-api:start-server :port 9090) - Iniciar en puerto personalizado~%")
(format t "  (mi-api:stop-server)            - Detener servidor~%")
(format t "  (mi-api:main)                   - Ejecutar función principal~%~%")
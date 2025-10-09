;;; src/package.lisp
(defpackage :mi-api
  (:use :cl :hunchentoot)
  (:export :start-server :stop-server :main
           :imgx-job-manager
           :procesar-lote-imgx
           :obtener-progreso
           :cancelar-procesamiento
           :obtener-estadisticas))
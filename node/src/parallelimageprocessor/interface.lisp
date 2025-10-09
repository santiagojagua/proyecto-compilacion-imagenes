;;; src/parallelimageprocessor/interface.lisp
(in-package :mi-api)

;; Interfaces/definiciones
(defclass imgx-job-manager ()
  ((lock         :accessor job-lock         :initform (bt:make-lock "imgx-job-lock"))
   (status       :accessor job-status       :initform :idle)
   (total        :accessor job-total        :initform 0)
   (done         :accessor job-done         :initform 0)
   (cancel-flag  :accessor job-cancel-flag  :initform nil)
   (thread       :accessor job-thread       :initform nil)
   (started-at   :accessor job-started-at   :initform nil)
   (ended-at     :accessor job-ended-at     :initform nil)
   (results      :accessor job-results      :initform nil)
   (id           :accessor job-id           :initform nil)))

;; firmas de genéricos (si ya existían con otro parámetro)
(defgeneric procesar-lote-imgx (manager tareas &key default-opts max-threads))
(defgeneric obtener-progreso (manager))
(defgeneric cancelar-procesamiento (manager))
(defgeneric obtener-estadisticas (manager))
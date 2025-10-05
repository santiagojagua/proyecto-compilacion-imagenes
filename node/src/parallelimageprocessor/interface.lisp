;;; src/parallelimageprocessor/interface.lisp
(in-package :mi-api)

;; Interfaces/definiciones

(defclass parallel-image-processor ()
  ((progreso :accessor progreso :initform 0.0)
   (procesando :accessor procesando :initform nil)
   (operaciones-completadas :accessor operaciones-completadas :initform 0)
   (total-operaciones :accessor total-operaciones :initform 0)
   (lock :reader lock :initform (bt:make-lock "processor-lock"))
   (worker-thread :accessor worker-thread :initform nil)))

(defstruct operacion
  id tipo parametros)

(defstruct resultado
  id exito datos error tiempo-ejecucion)

(defgeneric procesar-lote-paralelo (processor operaciones))
(defgeneric obtener-progreso (processor))
(defgeneric cancelar-procesamiento (processor))
(defgeneric obtener-estadisticas (processor))
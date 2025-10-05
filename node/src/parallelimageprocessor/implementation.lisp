;;; src/parallelimageprocessor/implementation.lisp
(in-package :mi-api)

(defun simular-procesamiento (operacion)
  (sleep (random 2.0))
  (make-resultado
   :id (operacion-id operacion)
   :exito t
   :datos `(("operacion" . ,(operacion-tipo operacion))
            ("resultado" . "exitoso"))
   :error nil
   :tiempo-ejecucion (random 3.0)))

(defmethod procesar-lote-paralelo ((processor parallel-image-processor) operaciones)
  (bt:with-lock-held ((lock processor))
    (when (procesando processor)
      (error "Ya hay un procesamiento en curso"))
    (setf (procesando processor) t
          (progreso processor) 0.0
          (operaciones-completadas processor) 0
          (total-operaciones processor) (length operaciones))
    (setf (worker-thread processor)
          (bt:make-thread
           (lambda ()
             (let ((resultados '()))
               (dolist (operacion operaciones)
                 (push (simular-procesamiento operacion) resultados)
                 (bt:with-lock-held ((lock processor))
                   (incf (operaciones-completadas processor))
                   (setf (progreso processor)
                         (/ (operaciones-completadas processor)
                            (float (total-operaciones processor)))))
                 (sleep 0.5))
               (bt:with-lock-held ((lock processor))
                 (setf (procesando processor) nil
                       (progreso processor) 1.0))
               (nreverse resultados)))
           :name "image-processor-worker"))
    `((:status . "procesamiento-iniciado")
      (:operaciones . ,(length operaciones))
      (:id-lote . ,(get-universal-time)))))

(defmethod obtener-progreso ((processor parallel-image-processor))
  (bt:with-lock-held ((lock processor))
    (progreso processor)))

(defmethod cancelar-procesamiento ((processor parallel-image-processor))
  (bt:with-lock-held ((lock processor))
    (when (and (procesando processor)
               (worker-thread processor)
               (bt:thread-alive-p (worker-thread processor)))
      (bt:destroy-thread (worker-thread processor)))
    (setf (procesando processor) nil
          (progreso processor) 0.0
          (worker-thread processor) nil)
    '((:status . "procesamiento-cancelado"))))

(defmethod obtener-estadisticas ((processor parallel-image-processor))
  (bt:with-lock-held ((lock processor))
    `((:procesando . ,(procesando processor))
      (:progreso . ,(progreso processor))
      (:operaciones-completadas . ,(operaciones-completadas processor))
      (:total-operaciones . ,(total-operaciones processor))
      (:timestamp . ,(get-universal-time)))))
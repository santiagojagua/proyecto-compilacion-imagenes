;;; src/parallelimageprocessor/implementation-imgx.lisp - Implementación del Job Manager IMGX
(in-package :mi-api)

(defun format-result (result)
  "Convierte un resultado a formato JSON-friendly"
  (let ((formatted `((:index . ,(getf result :index))
                     (:id . ,(getf result :id))
                     (:status . ,(string-downcase (symbol-name (getf result :status)))))))
    (case (getf result :status)
      (:ok
       (append formatted
               `((:ms . ,(getf result :ms)))
               (when (getf result :file-path)
                 `((:file-path . ,(getf result :file-path))))
               (when (getf result :output)
                 ;; Truncar base64 si es muy largo (opcional)
                 (let ((output (getf result :output)))
                   (if (stringp output)
                       `((:output . ,(if (> (length output) 100)
                                        (concatenate 'string 
                                                   (subseq output 0 100) 
                                                   "...")
                                        output)))
                       `((:output . ,output)))))))
      (:error
       (append formatted
               `((:message . ,(getf result :message)))))
      (t formatted))))

(defmethod procesar-lote-imgx ((mgr imgx-job-manager) tareas
                               &key (default-opts '()) max-threads)
  (bt:with-lock-held ((job-lock mgr))
    (when (eq (job-status mgr) :running)
      (error "Ya hay un procesamiento en curso; cancele antes de iniciar otro."))
    (setf (job-id mgr) (get-universal-time)
          (job-status mgr) :running
          (job-total mgr) (length tareas)
          (job-done mgr) 0
          (job-cancel-flag mgr) nil
          (job-started-at mgr) (get-universal-time)
          (job-ended-at mgr) nil
          (job-results mgr) nil)
    
    (setf (job-thread mgr)
          (bt:make-thread
           (lambda ()
             (let ((res (imgx-batch:procesar-peticion
                         tareas
                         :default-opts default-opts
                         :max-threads max-threads
                         :on-progress (lambda (&key index n &allow-other-keys)
                                        (declare (ignore index n))
                                        (bt:with-lock-held ((job-lock mgr))
                                          (when (eq (job-status mgr) :running)
                                            (incf (job-done mgr)))))
                         :cancel-predicate (lambda ()
                                             (bt:with-lock-held ((job-lock mgr))
                                               (job-cancel-flag mgr))))))
               (bt:with-lock-held ((job-lock mgr))
                 (setf (job-results mgr) res
                       (job-status mgr) (if (job-cancel-flag mgr) :cancelled :finished)
                       (job-ended-at mgr) (get-universal-time)))))
           :name "imgx-job")))
  
  ;; Respuesta inicial
  `((:status . "procesamiento-iniciado")
    (:operaciones . ,(length tareas))
    (:id-lote . ,(job-id mgr))))

(defmethod obtener-progreso ((mgr imgx-job-manager))
  (bt:with-lock-held ((job-lock mgr))
    (let* ((total (max 1 (job-total mgr)))
           (done  (job-done mgr))
           (prog  (min 1.0 (/ (float done) (float total))))
           (results (job-results mgr)))
      `((:status . ,(string-downcase (symbol-name (job-status mgr))))
        (:progreso . ,prog)
        (:completadas . ,done)
        (:total . ,total)
        (:id-lote . ,(job-id mgr))
        ;; NUEVO: Incluir resultados si el procesamiento terminó
        ,@(when (and results (member (job-status mgr) '(:finished :cancelled)))
            `((:resultados . ,(mapcar #'format-result results))))))))

(defmethod cancelar-procesamiento ((mgr imgx-job-manager))
  (bt:with-lock-held ((job-lock mgr))
    (cond
      ((not (eq (job-status mgr) :running))
       `((:status . "no-activo")
         (:id-lote . ,(job-id mgr))))
      (t
       (setf (job-cancel-flag mgr) t)
       `((:status . "cancelacion-solicitada")
         (:id-lote . ,(job-id mgr)))))))

(defmethod obtener-estadisticas ((mgr imgx-job-manager))
  (bt:with-lock-held ((job-lock mgr))
    `((:status . ,(string-downcase (symbol-name (job-status mgr))))
      (:id-lote . ,(job-id mgr))
      (:total . ,(job-total mgr))
      (:completadas . ,(job-done mgr))
      (:timestamp . ,(get-universal-time)))))
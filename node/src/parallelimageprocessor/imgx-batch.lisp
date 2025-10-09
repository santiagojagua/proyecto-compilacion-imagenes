;;;; src/parallelimageprocessor/imgx-batch.lisp
;;;; Procesamiento en paralelo de una petición con múltiples imágenes (Base64) usando hilos.
;;;; Depende de: bordeaux-threads y del módulo IMGX (imgx.lisp).
;;;; Exporta: IMGX-BATCH:PROCESAR-PETICION

(in-package #:cl-user)

(defpackage #:imgx-batch
  (:use #:cl)
  (:import-from #:imgx #:procesar-imagen-b64 #:guardar-imagen-b64)
  (:export #:procesar-peticion))
(in-package #:imgx-batch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bordeaux-threads)
    (error "Falta :bordeaux-threads. Carga: (ql:quickload :bordeaux-threads)")))

;; Alias corto
(defparameter *bt* (find-package :bordeaux-threads))

(defun %with-lock (lock thunk)
  (funcall (find-symbol "ACQUIRE-LOCK" *bt*) lock)
  (unwind-protect
      (funcall thunk)
    (funcall (find-symbol "RELEASE-LOCK" *bt*) lock)))

(defun %make-lock (&optional (name "imgx-batch-lock"))
  (funcall (find-symbol "MAKE-LOCK" *bt*) name))

(defun %make-thread (fn &optional (name "imgx-worker"))
  (funcall (find-symbol "MAKE-THREAD" *bt*) fn :name name))

(defun %join-thread (th)
  (funcall (find-symbol "JOIN-THREAD" *bt*) th))

;;;; Helpers

(defparameter +imgx-keys+
  '(:input-format :INPUT-FORMAT
    :grayscale :GRAYSCALE
    :resize :RESIZE
    :crop :CROP
    :rotate :ROTATE
    :flip :FLIP
    :blur :BLUR
    :sharpen :SHARPEN
    :brightness :BRIGHTNESS
    :contrast :CONTRAST
    :watermark-image :WATERMARK-IMAGE
    :watermark-image-b64 :WATERMARK-IMAGE-B64
    :output-format :OUTPUT-FORMAT :output--format :OUTPUT--FORMAT
    :quality :QUALITY
    :as-data-uri :AS-DATA-URI :as--data--uri :AS--DATA--URI
    :save-to-disk :SAVE-TO-DISK :save--to--disk :SAVE--TO--DISK
    :output-dir :OUTPUT-DIR :output--dir :OUTPUT--DIR
    :filename-prefix :FILENAME-PREFIX)
  "Claves aceptadas por IMGX:PROCESAR-IMAGEN-B64.")

(defun %filter-plist (plist allowed-keys)
  (loop for (k v) on plist by #'cddr
        when (member k allowed-keys)
        append (list k v)))

(defun %plist-merge (&rest plists)
  "Merge simple de plists. Claves posteriores sobrescriben a las anteriores."
  (let ((out '()))
    (dolist (pl plists)
      (loop for (k v) on pl by #'cddr do
            (setf (getf out k) v)))
    out))

(defun %extra-opts-from-task (task)
  "Devuelve los pares clave/valor del task que NO son :id, :input-b64, :b64 ni :ops."
  (loop for (k v) on task by #'cddr
        unless (member k '(:id :input-b64 :b64 :ops))
        append (list k v)))

(defun normalize-plist-keys (plist)
  "Normaliza todas las claves de un plist eliminando guiones duplicados"
  (loop for (k v) on plist by #'cddr
        for normalized = (if (keywordp k)
                            (intern (with-output-to-string (out)
                                      (loop with last-dash = nil
                                            for ch across (symbol-name k)
                                            do (if (char= ch #\-)
                                                   (unless last-dash
                                                     (write-char ch out)
                                                     (setf last-dash t))
                                                   (progn
                                                     (write-char ch out)
                                                     (setf last-dash nil)))))
                                    :keyword)
                            k)
        append (list normalized v)))

(defun %normalize-task (task default-opts)
  "Toma una tarea y devuelve tres valores"
  ;; Normalizar claves de la tarea primero
  (let* ((normalized-task (normalize-plist-keys task))
         (id  (getf normalized-task :id))
         (b64 (or (getf normalized-task :input-b64)
                  (getf normalized-task :b64)
                  (error "Tarea sin :input-b64 o :b64. Claves disponibles: ~{~S ~}"
                         (loop for (k v) on normalized-task by #'cddr collect k))))
         (ops (getf normalized-task :ops))
         ;; Normalizar ops si existe
         (normalized-ops (when ops (normalize-plist-keys ops)))
         (final-opts (%filter-plist
                      (%plist-merge default-opts normalized-ops)
                      +imgx-keys+)))
    
    (format t "~%DEBUG BATCH: ID=~S~%" id)
    (format t "DEBUG BATCH: B64 encontrado (primeros 50): ~S~%" 
            (subseq b64 0 (min 50 (length b64))))
    (format t "DEBUG BATCH: Final opts: ~S~%" final-opts)
    (values id b64 final-opts)))

(defun %process-one (task default-opts)
  "Procesa una sola tarea. Devuelve plist de resultado."
  (let ((start (get-internal-real-time)))
    (handler-case
        (multiple-value-bind (id b64 opts) (%normalize-task task default-opts)
          (let* ((result (apply #'imgx:procesar-imagen-b64 b64 opts))
                 (ms  (truncate (* 1000
                                   (/ (- (get-internal-real-time) start)
                                      internal-time-units-per-second)))))
            ;; result puede ser b64 o un plist con :output-b64 y :file-path
            (cond
              ((and (listp result) (getf result :file-path))
               (list :id id 
                     :status :ok 
                     :output (getf result :output-b64)
                     :file-path (namestring (getf result :file-path))
                     :ms ms))
              (t
               (list :id id :status :ok :output result :ms ms)))))
      (error (e)
        (list :id (getf task :id) :status :error :message (princ-to-string e))))))

(defun %default-nthreads (nitems)
  (min nitems 4))

;;;; API principal

(defun procesar-peticion (tareas
                          &key
                            (default-opts '())
                            (max-threads nil)
                            on-progress
                            (cancel-predicate (constantly nil)))
  "Procesa una lista de tareas en paralelo.
Cada tarea es un plist con al menos :input-b64 (o :b64).
Las opciones de edición pueden ir a nivel de tarea o bajo :ops.
Devuelve una lista de resultados en el mismo orden.

Ejemplos de tarea:
  '(:id 1 :input-b64 \"data:image/jpeg;base64,...\"
    :resize (800 600) :grayscale t :output-format :png :as-data-uri t
    :save-to-disk t :output-dir \"node/images/\")

  '(:id 2 :b64 \"...base64...\"
    :ops (:crop (100 100 400 300) :rotate 90 :quality 85 :output-format :jpg
          :save-to-disk t))"
  (let* ((vec (coerce tareas 'vector))
         (n   (length vec))
         (nthreads (or max-threads (%default-nthreads n)))
         (lock (%make-lock "imgx-batch-dispatch-lock"))
         (next 0)
         (results (make-array n :initial-element nil)))
    (labels ((take-index ()
           (%with-lock lock
             (lambda ()
               (if (or (funcall cancel-predicate)
                       (>= next n))
                   nil
                   (prog1 next (incf next)))))))
      (let ((threads
              (loop for i below nthreads
                    collect
                    (%make-thread
                     (lambda ()
                       (loop for idx = (take-index) while idx do
                         (let* ((task (aref vec idx))
                                (res  (%process-one task default-opts)))
                           (setf (aref results idx)
                                 (append (list :index idx) res))
                            (when on-progress
                                (funcall on-progress :index idx :n n)))))
                     (format nil "imgx-worker-~a" i)))))
        (dolist (th threads) (%join-thread th))
        (loop for i from 0 below n collect (aref results i))))))
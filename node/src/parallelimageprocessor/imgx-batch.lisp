;;;; imgx-batch.lisp
;;;; Procesamiento en paralelo de una petición con múltiples imágenes (Base64) usando hilos.
;;;; Depende de: bordeaux-threads y del módulo IMGX (imgx.lisp).
;;;; Exporta: IMGX-BATCH:PROCESAR-PETICION

(in-package #:cl-user)

(defpackage #:imgx-batch
  (:use #:cl)
  (:import-from #:imgx #:procesar-imagen-b64)
  (:export #:procesar-peticion))
(in-package #:imgx-batch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :bordeaux-threads)
    (error "Falta :bordeaux-threads. Carga: (ql:quickload :bordeaux-threads)")))

;; Alias corto
(defparameter *bt* (find-package :bordeaux-threads))

(defun %with-lock (lock thunk)
  (funcall (find-symbol "ACQUIRE-LOCK" *bt*) lock nil)
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
  '(:input-format :grayscale :resize :crop :rotate :flip :blur :sharpen
    :brightness :contrast :watermark-image :watermark-image-b64
    :output-format :quality :as-data-uri)
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

(defun %normalize-task (task default-opts)
  "Toma una tarea y devuelve tres valores:
   1) id (o NIL)
   2) b64 de entrada
   3) keyword args filtrados para IMGX:PROCESAR-IMAGEN-B64
   Levanta error si falta la imagen."
  (let* ((id  (getf task :id))
         (b64 (or (getf task :input-b64)
                  (getf task :b64)
                  (error "Tarea sin :input-b64/:b64.")))
         (ops (or (getf task :ops)
                  (%extra-opts-from-task task)))
         (final-opts (%filter-plist
                      (%plist-merge default-opts ops)
                      +imgx-keys+)))
    (values id b64 final-opts)))

(defun %process-one (task default-opts)
  "Procesa una sola tarea. Devuelve plist de resultado."
  (let ((start (get-internal-real-time)))
    (handler-case
        (multiple-value-bind (id b64 opts) (%normalize-task task default-opts)
          (let* ((out (apply #'imgx:procesar-imagen-b64 b64 opts))
                 (ms  (truncate (* 1000
                                   (/ (- (get-internal-real-time) start)
                                      internal-time-units-per-second)))))
            (list :id id :status :ok :output out :ms ms)))
      (error (e)
        (list :id (getf task :id) :status :error :message (princ-to-string e))))))

(defun %default-nthreads (nitems)
  (min nitems 4)) ;; Ajusta este valor por defecto si lo deseas

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
    :resize (800 600) :grayscale t :output-format :png :as-data-uri t)

  '(:id 2 :b64 \"...base64...\"
    :ops (:crop (100 100 400 300) :rotate 90 :quality 85 :output-format :jpg))"
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
                           ;; Guardar manteniendo orden
                           (setf (aref results idx)
                                 (append (list :index idx) res))
                            (when on-progress
                                (funcall on-progress :index idx :n n)))))
                     (format nil "imgx-worker-~a" i)))))
        ;; Esperar a que terminen
        (dolist (th threads) (%join-thread th))
        ;; Convertir a lista y devolver
        (loop for i from 0 below n collect (aref results i))))))
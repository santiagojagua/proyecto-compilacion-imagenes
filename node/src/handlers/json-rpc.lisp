;;; src/handlers/json-rpc.lisp - Handlers JSON-RPC para ParallelImageProcessor
(in-package :mi-api)

;; Evitar style-warnings si se compila antes que main.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (special image-processor)))

(defun parse-json-request ()
  "Parsea el cuerpo de la petición JSON"
  (let ((content (raw-post-data :force-text t)))
    (when (and content (> (length content) 0))
      (cl-json:decode-json-from-string content))))

(defun create-json-response (result &key id)
  "Crea una respuesta JSON-RPC"
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string
   `(("jsonrpc" . "2.0")
     ("result" . ,result)
     ("id" . ,(or id 1)))))

(defun create-error-response (code message &key id)
  "Crea una respuesta de error JSON-RPC"
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string
   `(("jsonrpc" . "2.0")
     ("error" . (("code" . ,code)
                 ("message" . ,message)))
     ("id" . ,(or id 1)))))

(defun alist-p (x)
  "Devuelve T si x es una lista de cons (alist)."
  (and (listp x) (every #'consp x)))

(defun jassoc (key obj)
  "assoc tolerante a claves keyword o string; solo si obj es alist."
  (when (listp obj)
    (or (assoc key obj)
        (and (keywordp key)
             (or (assoc (symbol-name key) obj :test #'equal)
                 (assoc (string-downcase (symbol-name key)) obj :test #'equal)))
        (and (stringp key)
             (let ((k (ignore-errors (intern (string-upcase key) :keyword))))
               (and k (assoc k obj)))))))

(defun jget (obj key)
  "Obtiene el valor para key (keyword o string) de un alist."
  (let ((cell (jassoc key obj)))
    (when cell (cdr cell))))

(defun convert-to-operaciones (operaciones-json)
  "Convierte operaciones JSON a objetos operacion (soporta vector o lista)."
  (let ((ops (cond
               ((vectorp operaciones-json) (coerce operaciones-json 'list))
               ((listp operaciones-json) operaciones-json)
               (t nil))))
    (mapcar (lambda (op-json)
              (make-operacion
               :id (jget op-json :id)
               :tipo (jget op-json :tipo)
               :parametros (jget op-json :parametros)))
            (or ops '()))))

(define-easy-handler (json-rpc-handler :uri "/api/rpc" :default-request-type :post) ()
  (handler-case
      (let* ((request (parse-json-request))
             (method (or (jget request :method) (jget request "method")))
             (params (or (jget request :params) (jget request "params")))
             (id (or (jget request :id) (jget request "id"))))
        (format t "📥 JSON-RPC Request: ~A~%" method)
        (cond
          ((string= (or method "") "procesarLoteParalelo")
           ;; params puede ser:
           ;; - vector de operaciones (params como array)
           ;; - alist con clave :operaciones / "operaciones"
           ;; - lista de operaciones (si *json-array-type* fuese 'list)
           (let* ((ops-raw (cond
                             ((vectorp params) params)
                             ((alist-p params)
                              (or (jget params :operaciones)
                                  (jget params "operaciones")))
                             ((listp params) params)
                             (t nil)))
                  (ops-list (cond
                              ((vectorp ops-raw) (coerce ops-raw 'list))
                              ((listp ops-raw) ops-raw)
                              (t nil))))
             (if ops-list
                 (create-json-response
                  (procesar-lote-paralelo
                   image-processor
                   (convert-to-operaciones ops-list))
                  :id id)
                 (create-error-response -32602
                   "Parámetros inválidos: se esperaba 'operaciones' como lista/array"
                   :id id))))
          ((string= (or method "") "obtenerProgreso")
           (create-json-response
            (obtener-progreso image-processor) :id id))
          ((string= (or method "") "cancelarProcesamiento")
           (create-json-response
            (cancelar-procesamiento image-processor) :id id))
          ((string= (or method "") "obtenerEstadisticas")
           (create-json-response
            (obtener-estadisticas image-processor) :id id))
          (t
           (create-error-response -32601 "Método no encontrado" :id id))))
    (error (e)
      (create-error-response -32603
                             (format nil "Error interno del servidor: ~A" e)))))
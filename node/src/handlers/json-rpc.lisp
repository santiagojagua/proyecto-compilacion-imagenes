;;; src/handlers/json-rpc.lisp - JSON-RPC con IMGX Job Manager
(in-package :mi-api)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (special job-manager)))

(defun parse-json-request ()
  (let ((content (raw-post-data :force-text t)))
    (when (and content (> (length content) 0))
      (cl-json:decode-json-from-string content))))

(defun create-json-response (result &key id)
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string
   `(("jsonrpc" . "2.0")
     ("result" . ,result)
     ("id" . ,(or id 1)))))

(defun create-error-response (code message &key id data)
  (setf (content-type*) "application/json; charset=utf-8")
  (cl-json:encode-json-to-string
   `(("jsonrpc" . "2.0")
     ("error" . ,(append `(("code" . ,code) ("message" . ,message))
                         (when data `(("data" . ,data)))))
     ("id" . ,(or id 1)))))

(defun safe-backtrace ()
  (ignore-errors
    (with-output-to-string (s)
      (let ((pkg (find-package :sb-debug)))
        (when pkg
          (let ((fn (find-symbol "BACKTRACE" pkg)))
            (when fn (funcall fn 50 s))))))))

(defun alist-p (x)
  (and (listp x) (every #'consp x)))

(defun jassoc (key obj)
  (when (listp obj)
    (or (assoc key obj)
        (and (keywordp key)
             (or (assoc (symbol-name key) obj :test #'equal)
                 (assoc (string-downcase (symbol-name key)) obj :test #'equal)))
        (and (stringp key)
             (let ((k (ignore-errors (intern (string-upcase key) :keyword))))
               (and k (assoc k obj)))))))

(defun jget (obj key)
  (let ((cell (jassoc key obj)))
    (when cell (cdr cell))))

(defun to-keyword (k)
  (cond
    ((keywordp k) k)
    ((symbolp k) (intern (string-upcase (symbol-name k)) :keyword))
    ((stringp k) (intern (string-upcase k) :keyword))
    (t k)))

(defun deep-json->lisp (x)
  (cond
    ((vectorp x) (map 'list #'deep-json->lisp x))
    ((alist-p x) (loop for (k . v) in x
                       append (list (to-keyword k) (deep-json->lisp v))))
    (t x)))

(define-easy-handler (json-rpc-handler :uri "/api/rpc" :default-request-type :post) ()
  (handler-case
      (let* ((request (parse-json-request))
             (method (or (jget request :method) (jget request "method")))
             (params (or (jget request :params) (jget request "params")))
             (id (or (jget request :id) (jget request "id"))))
        (cond
          ((string= (or method "") "imgxProcesarLote")
           (let* ((tasks-raw (cond
                               ((vectorp params) params)
                               ((alist-p params)
                                (or (jget params :tareas) (jget params "tareas")
                                    (jget params :tasks)  (jget params "tasks")
                                    params))
                               ((listp params) params)
                               (t nil)))
                  (tareas (mapcar #'deep-json->lisp
                                  (cond
                                    ((vectorp tasks-raw) (coerce tasks-raw 'list))
                                    ((listp tasks-raw) tasks-raw)
                                    ((alist-p tasks-raw) (list tasks-raw))
                                    (t '()))))
                  (default-opts (and (alist-p params)
                                     (deep-json->lisp
                                      (or (jget params :default-opts)
                                          (jget params "default-opts")
                                          (jget params :defaultOpts)
                                          (jget params "defaultOpts")))))
                  (max-threads (and (alist-p params)
                                    (or (jget params :max-threads)
                                        (jget params "max-threads")
                                        (jget params :maxThreads)
                                        (jget params "maxThreads")))))
             (if (null tareas)
                 (create-error-response -32602 "Parámetros inválidos: se esperaban 'tareas' como lista/array" :id id)
                 (create-json-response
                  (mi-api:procesar-lote-imgx job-manager tareas
                                             :default-opts (or default-opts '())
                                             :max-threads max-threads)
                  :id id))))
          ((string= (or method "") "obtenerProgreso")
           (create-json-response (mi-api:obtener-progreso job-manager) :id id))
          ((string= (or method "") "cancelarProcesamiento")
           (create-json-response (mi-api:cancelar-procesamiento job-manager) :id id))
          ((string= (or method "") "obtenerEstadisticas")
           (create-json-response (mi-api:obtener-estadisticas job-manager) :id id))
          (t
           (create-error-response -32601 "Método no encontrado" :id id))))
    (error (e)
      (let* ((rid (ignore-errors
                    (let ((req (parse-json-request)))
                      (or (jget req :id) (jget req "id")))))
             (bt (safe-backtrace)))
        (create-error-response -32603
                               (format nil "Error interno del servidor: ~A" e)
                               :id rid
                               :data bt)))))
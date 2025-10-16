;;;; src/handlers/json-rpc.lisp - JSON-RPC con IMGX Job Manager
(in-package :mi-api)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (special job-manager)))

;;; ============================================================================
;;; Utilidades para conversión de listas de caracteres a strings
;;; ============================================================================

(defun convert-char-lists-to-strings (obj)
  "Convierte recursivamente todas las listas/vectores de caracteres en strings.
   CL-JSON a veces retorna strings como listas o vectores de caracteres."
  (cond
    ;; Vector de caracteres -> string
    ((and (vectorp obj) (every #'characterp obj))
     (coerce obj 'string))
    
    ;; Lista de caracteres -> string
    ((and (listp obj) (not (null obj)) (every #'characterp obj))
     (coerce obj 'string))
    
    ;; Cons cell (par key . value)
    ((and (consp obj) (not (listp (cdr obj))))
     (cons (convert-char-lists-to-strings (car obj))
           (convert-char-lists-to-strings (cdr obj))))
    
    ;; Lista -> procesar cada elemento
    ((listp obj)
     (mapcar #'convert-char-lists-to-strings obj))
    
    ;; Vector -> procesar cada elemento
    ((vectorp obj)
     (map 'vector #'convert-char-lists-to-strings obj))
    
    ;; Otro tipo -> devolver tal cual
    (t obj)))

;;; ============================================================================
;;; Funciones de parsing y respuesta JSON-RPC
;;; ============================================================================

(defun parse-json-request ()
  "Parse JSON request y convierte listas de caracteres a strings"
  (let ((content (raw-post-data :force-text t)))
    (when (and content (> (length content) 0))
      (let ((parsed (cl-json:decode-json-from-string content)))
        (convert-char-lists-to-strings parsed)))))

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

;;; ============================================================================
;;; Utilidades para trabajar con alists y plists
;;; ============================================================================

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

(defun normalize-keyword-name (str)
  "Normaliza un string de keyword: sustituye _ por -, elimina guiones duplicados"
  (let ((step1 (substitute #\- #\_ str)))
    (with-output-to-string (out)
      (loop with last-dash = nil
            for ch across step1
            do (if (char= ch #\-)
                   (unless last-dash
                     (write-char ch out)
                     (setf last-dash t))
                   (progn
                     (write-char ch out)
                     (setf last-dash nil)))))))

(defun string-to-keyword (str)
  "Convierte string a keyword normalizando guiones"
  (intern (string-upcase (normalize-keyword-name str)) :keyword))

(defun deep-json->lisp (x)
  "Convierte JSON (alists) a plists de Lisp, normalizando claves Y CONVIRTIENDO LISTAS DE CHARS"
  (cond
    ;; Vector de caracteres -> string
    ((and (vectorp x) (> (length x) 0) (every #'characterp x))
     (coerce x 'string))
    
    ;; Lista de caracteres -> string (ANTES de otros checks de lista)
    ((and (listp x) (not (null x)) (every #'characterp x))
     (coerce x 'string))
    
    ;; Vector -> lista procesada
    ((vectorp x)
     (map 'list #'deep-json->lisp x))
    
    ;; Alist -> plist (procesando valores recursivamente)
    ((and (listp x) (not (null x)) (every #'consp x))
     (loop for (k . v) in x
           for key = (cond
                       ((stringp k) (string-to-keyword k))
                       ((symbolp k) (intern (string-upcase (symbol-name k)) :keyword))
                       (t k))
           ;; IMPORTANTE: Aplicar deep-json->lisp al valor para convertir char-lists
           for val = (deep-json->lisp v)
           append (list key val)))
    
    ;; Lista normal (no alist, no chars)
    ((listp x)
     (mapcar #'deep-json->lisp x))
    
    ;; Valor simple
    (t x)))
;;; ============================================================================
;;; Handler JSON-RPC principal
;;; ============================================================================

(define-easy-handler (json-rpc-handler :uri "/api/rpc" :default-request-type :post) ()
  (handler-case
      (let* ((request (parse-json-request))
             (method (or (jget request :method) (jget request "method")))
             (params (or (jget request :params) (jget request "params")))
             (id (or (jget request :id) (jget request "id"))))
        
        (cond
          ;;; Método: imgxProcesarLote
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
             
             ;; Asegurar save-to-disk por defecto
             (unless (getf default-opts :save-to-disk)
               (setf default-opts (append default-opts '(:save-to-disk t))))
             
             (if (null tareas)
                 (create-error-response -32602 
                                       "Parámetros inválidos: se esperaban 'tareas' como lista/array" 
                                       :id id)
                 (create-json-response
                  (mi-api:procesar-lote-imgx job-manager tareas
                                             :default-opts (or default-opts '())
                                             :max-threads max-threads)
                  :id id))))
          
          ;;; Método: obtenerProgreso
          ((string= (or method "") "obtenerProgreso")
           (create-json-response (mi-api:obtener-progreso job-manager) :id id))
          
          ;;; Método: cancelarProcesamiento
          ((string= (or method "") "cancelarProcesamiento")
           (create-json-response (mi-api:cancelar-procesamiento job-manager) :id id))
          
          ;;; Método: obtenerEstadisticas
          ((string= (or method "") "obtenerEstadisticas")
           (create-json-response (mi-api:obtener-estadisticas job-manager) :id id))
          
          ;;; Método no encontrado
          (t
           (create-error-response -32601 "Método no encontrado" :id id))))
    
    ;; Manejo de errores
    (error (e)
      (let* ((rid (ignore-errors
                    (let ((req (parse-json-request)))
                      (or (jget req :id) (jget req "id")))))
             (bt (safe-backtrace)))
        (create-error-response -32603
                               (format nil "Error interno del servidor: ~A" e)
                               :id rid
                               :data bt)))))
;;;; imgx.lisp
;;;; Módulo de procesamiento de imágenes con OptiCL y entrada/salida Base64 (cl-base64).
;;;; Exporta: IMGX:PROCESAR-IMAGEN-B64

(in-package #:cl-user)

(defpackage #:imgx
  (:use #:cl)
  (:export #:procesar-imagen-b64))
(in-package #:imgx)

;;;; Utilidades de formato y Base64

(defun %ext (path)
  (string-downcase (or (pathname-type path) "")))

(defun %format->ext (fmt)
  (ecase fmt
    (:jpg "jpg")
    (:png "png")
    (:tif "tif")))

(defun %mime->format (mime)
  (when mime
    (cond
      ((string= mime "image/jpeg") :jpg)
      ((string= mime "image/jpg")  :jpg)
      ((string= mime "image/png")  :png)
      ((string= mime "image/tiff") :tif)
      (t nil))))

(defun %detect-format-bytes (bytes)
  (let ((len (length bytes)))
    (labels ((b (i) (if (< i len) (aref bytes i) -1)))
      (cond
        ;; PNG signature
        ((and (>= len 8)
              (= (b 0) #x89) (= (b 1) #x50) (= (b 2) #x4E) (= (b 3) #x47)
              (= (b 4) #x0D) (= (b 5) #x0A) (= (b 6) #x1A) (= (b 7) #x0A))
         :png)
        ;; JPEG SOI
        ((and (>= len 3)
              (= (b 0) #xFF) (= (b 1) #xD8) (= (b 2) #xFF))
         :jpg)
        ;; TIFF "II*\0" o "MM\0*"
        ((and (>= len 4)
              (or (and (= (b 0) #x49) (= (b 1) #x49) (= (b 2) #x2A) (= (b 3) #x00))
                  (and (= (b 0) #x4D) (= (b 1) #x4D) (= (b 2) #x00) (= (b 3) #x2A))))
         :tif)
        (t nil)))))

(defun %tmpdir ()
  (or (ignore-errors
        (when (find-package :uiop)
          (let ((f (find-symbol "TMPDIR" :uiop)))
            (when (and f (fboundp f)) (funcall f)))))
      #+unix (pathname "/tmp/")
      (user-homedir-pathname)))

(defun %make-temp-path (fmt)
  (let* ((dir (%tmpdir))
         (name (format nil "imgx-~36r-~d.~a"
                       (random (expt 36 8)) (get-universal-time) (%format->ext fmt))))
    (merge-pathnames name (if (pathnamep dir) dir (pathname dir)))))

(defun %write-bytes-to-file (bytes path)
  (with-open-file (out path :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (write-sequence bytes out)
    path))

(defun %read-file-to-bytes (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun %strip-data-uri (s)
  "Devuelve dos valores: (base64-solo, mime-type-o-nil) si S es data URI; en otro caso (S, nil)."
  (if (and s (>= (length s) 5) (string= "data:" s :end2 5))
      (let* ((comma (or (position #\, s) (error "Data URI inválida.")))
             (meta (subseq s 5 comma))
             (b64  (subseq s (1+ comma))))
        (values b64 (string-downcase (subseq meta 0 (or (position #\; meta) (length meta))))))
      (values s nil)))

(defun %b64->bytes (b64)
  (let* ((pkg (or (find-package :cl-base64)
                  (error "Falta CL-BASE64. Carga: (ql:quickload :cl-base64)")))
         (fn  (or (find-symbol "BASE64-STRING-TO-USB8-ARRAY" pkg)
                  (error "CL-BASE64 no expone BASE64-STRING-TO-USB8-ARRAY."))))
    (funcall fn b64)))

(defun %bytes->b64 (bytes)
  (let* ((pkg (or (find-package :cl-base64)
                  (error "Falta CL-BASE64. Carga: (ql:quickload :cl-base64)")))
         (fn  (or (find-symbol "USB8-ARRAY-TO-BASE64-STRING" pkg)
                  (error "CL-BASE64 no expone USB8-ARRAY-TO-BASE64-STRING."))))
    (funcall fn bytes)))

;;;; Lectura/escritura con OptiCL (por archivo)

(defun %leer-imagen (path)
  (let ((ext (%ext path)))
    (cond
      ((member ext '("jpg" "jpeg") :test #'string=)
       (let ((f (find-symbol "READ-JPEG-FILE" :opticl)))
         (if (and f (fboundp f)) (funcall f path)
             (error "OptiCL no tiene READ-JPEG-FILE en esta versión."))))
      ((string= ext "png")
       (let ((f (find-symbol "READ-PNG-FILE" :opticl)))
         (if (and f (fboundp f)) (funcall f path)
             (error "OptiCL no tiene READ-PNG-FILE en esta versión."))))
      ((member ext '("tif" "tiff") :test #'string=)
       (let ((f (find-symbol "READ-TIFF-FILE" :opticl)))
         (if (and f (fboundp f)) (funcall f path)
             (error "OptiCL no tiene READ-TIFF-FILE en esta versión."))))
      (t (error "Formato de entrada no soportado: ~a" ext)))))

(defun %escribir-imagen (img path &key format (quality 90))
  (let* ((ext (%ext path))
         (fmt (or format
                  (cond
                    ((member ext '("jpg" "jpeg") :test #'string=) :jpg)
                    ((string= ext "png") :png)
                    ((member ext '("tif" "tiff") :test #'string=) :tif)
                    (t (error "No puedo deducir formato de salida por extensión: ~a" ext))))))
    (ecase fmt
      (:jpg
       (let ((f (find-symbol "WRITE-JPEG-FILE" :opticl)))
         (if (and f (fboundp f))
             (funcall f path img :quality quality)
             (error "OptiCL no tiene WRITE-JPEG-FILE en esta versión."))))
      (:png
       (let ((f (find-symbol "WRITE-PNG-FILE" :opticl)))
         (if (and f (fboundp f)) (funcall f path img)
             (error "OptiCL no tiene WRITE-PNG-FILE en esta versión."))))
      (:tif
       (let ((f (find-symbol "WRITE-TIFF-FILE" :opticl)))
         (if (and f (fboundp f)) (funcall f path img)
             (error "OptiCL no tiene WRITE-TIFF-FILE en esta versión.")))))))

;;;; Wrappers de operaciones (OptiCL)

(defun %rgb->gray (img)
  (let ((f (or (find-symbol "RGB->GRAY" :opticl)
               (find-symbol "RGB-TO-GRAY" :opticl))))
    (if (and f (fboundp f)) (funcall f img)
        (error "Esta versión de OptiCL no expone RGB->GRAY / RGB-TO-GRAY."))))

(defun %resize (img w h &key method)
  (declare (ignore method))
  (let ((f (find-symbol "RESIZE" :opticl)))
    (if (and f (fboundp f)) (funcall f img w h)
        (error "OptiCL no tiene RESIZE en esta versión."))))

(defun %crop (img x y w h)
  (let ((f (find-symbol "CROP" :opticl)))
    (if (and f (fboundp f)) (funcall f img x y w h)
        (error "OptiCL no tiene CROP en esta versión."))))

(defun %rotate (img angle)
  (let ((f (or (find-symbol "ROTATE" :opticl)
               (find-symbol "ROTATE-IMAGE" :opticl))))
    (if (and f (fboundp f)) (funcall f img angle)
        (cond
          ((member angle '(0 360)) img)
          ((member angle '(90 180 270))
           (let ((f90 (find-symbol (format nil "ROTATE-~A" angle) :opticl)))
             (if (and f90 (fboundp f90)) (funcall f90 img)
                 (error "No hay ROTATE ni ROTATE-~A en tu OptiCL." angle))))
          (t (error "Tu OptiCL no soporta rotación arbitraria."))))))

(defun %flip (img dir)
  (ecase dir
    (:horizontal
     (let ((f (or (find-symbol "FLIP-HORIZONTAL" :opticl)
                  (find-symbol "MIRROR-HORIZONTAL" :opticl))))
       (if (and f (fboundp f)) (funcall f img)
           (error "OptiCL no tiene FLIP-HORIZONTAL/MIRROR-HORIZONTAL."))))
    (:vertical
     (let ((f (or (find-symbol "FLIP-VERTICAL" :opticl)
                  (find-symbol "MIRROR-VERTICAL" :opticl))))
       (if (and f (fboundp f)) (funcall f img)
           (error "OptiCL no tiene FLIP-VERTICAL/MIRROR-VERTICAL."))))))

(defun %gaussian-blur (img sigma &key radius)
  (declare (ignore radius))
  (let ((f (or (find-symbol "GAUSSIAN-BLUR" :opticl)
               (find-symbol "BLUR-GAUSSIAN" :opticl))))
    (if (and f (fboundp f)) (funcall f img sigma)
        (error "No encuentro GAUSSIAN-BLUR en tu OptiCL."))))

(defun %unsharp-mask (img &key (radius 1.0) (amount 1.0) (threshold 0))
  (declare (ignore radius threshold))
  (let ((f (find-symbol "UNSHARP-MASK" :opticl)))
    (if (and f (fboundp f))
        (funcall f img amount)
        (error "No encuentro UNSHARP-MASK en tu OptiCL."))))

(defun %brightness-contrast (img &key brightness contrast)
  (let ((f (or (find-symbol "ADJUST-BRIGHTNESS-CONTRAST" :opticl)
               (find-symbol "BRIGHTNESS-CONTRAST" :opticl))))
    (if (and f (fboundp f))
        (funcall f img :brightness (or brightness 0) :contrast (or contrast 1.0))
        (error "Tu OptiCL no tiene ajuste de brillo/contraste integrado."))))

(defun %composite-over (base overlay x y &key (alpha 1.0))
  (let ((f (or (find-symbol "COMPOSITE-OVER" :opticl)
               (find-symbol "OVERLAY" :opticl))))
    (if (and f (fboundp f)) (funcall f base overlay x y :alpha alpha)
        (error "Tu OptiCL no tiene COMPOSITE-OVER/OVERLAY."))))

(defun %watermark-image (img wm-spec)
  (destructuring-bind (wm-path &key (x 0) (y 0) (alpha 1.0)) wm-spec
    (let ((wm (%leer-imagen wm-path)))
      (%composite-over img wm x y :alpha alpha))))

(defun %watermark-image-b64 (img wm-spec)
  "wm-spec = (b64-string :format :png|:jpg|:tif :x 0 :y 0 :alpha 1.0)
   También acepta data URI en b64-string, y deduce formato."
  (destructuring-bind (b64 &key format (x 0) (y 0) (alpha 1.0)) wm-spec
    (multiple-value-bind (pure-b64 mime) (%strip-data-uri b64)
      (let* ((bytes (%b64->bytes pure-b64))
             (fmt   (or format (%mime->format mime) (%detect-format-bytes bytes)
                        (error "No puedo deducir formato de la marca de agua en base64.")))
             (tmp   (%make-temp-path fmt)))
        (unwind-protect
             (progn
               (%write-bytes-to-file bytes tmp)
               (let ((wm (%leer-imagen tmp)))
                 (%composite-over img wm x y :alpha alpha)))
          (ignore-errors
            (when (probe-file tmp) (delete-file tmp))))))))

;;;; Pipeline por archivo (interno, pero útil)
(defun %procesar-imagen-archivo (entrada salida
                                &key
                                  grayscale
                                  resize               ;; (w h)
                                  crop                 ;; (x y w h)
                                  rotate               ;; 0|90|180|270 o grados arbitrarios si disponible
                                  flip                 ;; :horizontal | :vertical | (:horizontal :vertical)
                                  blur                 ;; real o (:sigma s :radius r)
                                  sharpen              ;; t o (:radius r :amount a :threshold t)
                                  brightness
                                  contrast
                                  watermark-image      ;; ("logo.png" :x 10 :y 10 :alpha 0.3)
                                  format               ;; :jpg | :png | :tif
                                  (quality 90))
  (let ((img (%leer-imagen entrada)))
    (when grayscale
      (setf img (%rgb->gray img)))
    (when resize
      (destructuring-bind (w h &rest rest) resize
        (declare (ignore rest))
        (setf img (%resize img w h))))
    (when crop
      (destructuring-bind (x y w h) crop
        (setf img (%crop img x y w h))))
    (when rotate
      (setf img (%rotate img rotate)))
    (when flip
      (etypecase flip
        (symbol (setf img (%flip img flip)))
        (list (dolist (f flip) (setf img (%flip img f))))))
    (when blur
      (etypecase blur
        (real (setf img (%gaussian-blur img blur)))
        (list (destructuring-bind (&key (sigma 1.0) radius) blur
                (setf img (%gaussian-blur img sigma :radius radius))))))
    (when sharpen
      (etypecase sharpen
        (boolean (when sharpen (setf img (%unsharp-mask img))))
        (list (setf img (apply #'%unsharp-mask img sharpen)))))
    (when (or brightness contrast)
      (setf img (%brightness-contrast img :brightness brightness :contrast contrast)))
    (when watermark-image
      (setf img (%watermark-image img watermark-image)))
    (%escribir-imagen img salida :format format :quality quality)
    salida))

;;;; API pública: entrada y salida en Base64
(defun procesar-imagen-b64 (entrada-b64
                            &key
                              input-format        ;; :jpg | :png | :tif (opcional, se intenta deducir)
                              grayscale
                              resize
                              crop
                              rotate
                              flip
                              blur
                              sharpen
                              brightness
                              contrast
                              watermark-image      ;; ("logo.png" :x :y :alpha)
                              watermark-image-b64  ;; (b64 :format :png :x :y :alpha) o data URI
                              output-format        ;; :jpg | :png | :tif (si no, igual al input)
                              (quality 90)
                              (as-data-uri nil))   ;; si T, devuelve data URI
  "Procesa una imagen codificada en Base64 (o Data URI) y devuelve el resultado en Base64.
Parámetros principales:
- :resize '(W H)
- :crop   '(X Y W H)
- :rotate 0|90|180|270 o grados arbitrarios si OptiCL lo soporta
- :flip   :horizontal | :vertical | (:horizontal :vertical)
- :blur   1.2 o '(:sigma 1.2 :radius 3)
- :sharpen t o '(:radius 2.0 :amount 1.0 :threshold 0)
- :brightness (por tu versión de OptiCL)
- :contrast   (1.0 = sin cambio)
- :watermark-image '(\"logo.png\" :x 20 :y 20 :alpha 0.3)
- :watermark-image-b64 '(\"data:image/png;base64,...\" :x 20 :y 20 :alpha 0.3)
- :output-format :jpg | :png | :tif
- :as-data-uri t => devuelve 'data:image/xxx;base64,...'"
  (multiple-value-bind (pure-b64 mime) (%strip-data-uri entrada-b64)
    (let* ((bytes (%b64->bytes pure-b64))
           (fmt-in (or input-format (%mime->format mime) (%detect-format-bytes bytes)
                       (error "No puedo deducir formato de entrada (JPG/PNG/TIF).")))
           (tmp-in  (%make-temp-path fmt-in))
           (fmt-out (or output-format fmt-in))
           (tmp-out (%make-temp-path fmt-out)))
      (unwind-protect
           (progn
             (%write-bytes-to-file bytes tmp-in)
             ;; Procesamos hasta escribir archivo de salida
             (%procesar-imagen-archivo
              tmp-in tmp-out
              :grayscale grayscale
              :resize resize
              :crop crop
              :rotate rotate
              :flip flip
              :blur blur
              :sharpen sharpen
              :brightness brightness
              :contrast contrast
              :watermark-image watermark-image ; si viene b64, la aplicamos luego
              :format fmt-out
              :quality quality)
             ;; Si hay marca de agua en base64, la aplicamos ahora
             (when watermark-image-b64
               (let ((img (%leer-imagen tmp-out)))
                 (%watermark-image-b64 img watermark-image-b64)
                 (%escribir-imagen img tmp-out :format fmt-out :quality quality)))
             ;; Leemos bytes de salida y devolvemos base64 (o data URI)
             (let* ((out-bytes (%read-file-to-bytes tmp-out))
                    (b64 (%bytes->b64 out-bytes)))
               (if as-data-uri
                   (format nil "data:image/~a;base64,~a"
                           (%format->ext fmt-out) b64)
                   b64)))
        (ignore-errors (when (probe-file tmp-in)  (delete-file tmp-in)))
        (ignore-errors (when (probe-file tmp-out) (delete-file tmp-out)))))))
;;; mi-api.asd
(asdf:defsystem #:mi-api
  :description "API simple en Common Lisp con Hunchentoot y JSON-RPC"
  :version "1.0.0"
  :author "jagua"
  :license "MIT"
  :depends-on (#:hunchentoot #:cl-json #:alexandria #:bordeaux-threads #:cl-base64 #:opticl)
  :components
  ((:file "src/package")
   (:module "src"
    :serial t
    :components
    ((:module "parallelimageprocessor"
      :serial t
      :pathname "parallelimageprocessor/"
      :components
      ((:file "imgx")
       (:file "imgx-batch")
       (:file "interface")
       (:file "implementation-imgx")))
     (:module "handlers"
      :serial t
      :pathname "handlers/"
      :components
      ((:file "basic")
       (:file "json-rpc")))
     (:file "main")))))
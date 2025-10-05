;;; mi-api.asd
(asdf:defsystem #:mi-api
  :description "API simple en Common Lisp con Hunchentoot y JSON-RPC"
  :version "1.0.0"
  :author "jagua"
  :license "MIT"
  :depends-on (#:hunchentoot #:cl-json #:alexandria #:bordeaux-threads)
  :components
  ((:module "src"
    :components
    ((:file "main")
     (:module "parallelimageprocessor"
      :components
      ((:file "interface")
       (:file "implementation")))
     (:module "handlers"
      :components
      ((:file "basic")
       (:file "json-rpc")))))))
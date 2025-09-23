;;; mi-api.asd - Sistema ASDF para la API
(asdf:defsystem #:mi-api
  :description "API simple en Common Lisp con Hunchentoot"
  :version "1.0.0"
  :author "jagua"
  :license "MIT"
  :depends-on (#:hunchentoot 
               #:cl-json
               #:alexandria)
  :components ((:file "src/main")))

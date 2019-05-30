;;;; snake.asd

(asdf:defsystem #:snake
  :description "Simple snake game."
  :author "Brad Svercl"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:alexandria #:bodge-math)
  :components ((:file "package")
               (:file "snake")))

;;;;
;;;; System definition for the ks2 SyGuS solver plugin
;;;;
(asdf:defsystem "systems.duck.ks2.sygus"
  :description "Plugin for the ks2 synthesizer suite for using standalone SyGuS solvers"
  :version "0.0.1"
  :author "Keith Johnson <quack@duck.systems>"
  :license "MIT"
  :depends-on ("com.kjcjohnson.ks2/solver-api"
               "com.kjcjohnson.synthkit/semgus")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "chc")
               (:file "synth-fun")
               (:file "spec")
               (:file "sygus-info")
               (:file "solver")))

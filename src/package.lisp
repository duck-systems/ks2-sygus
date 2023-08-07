;;;;
;;;; ks2 SyGuS package
;;;;
(defpackage #:systems.duck.ks2.sygus
  (:use #:cl #:com.kjcjohnson.ks2.solver-api)
  (:shadow #:variable #:sort)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:chc #:com.kjcjohnson.synthkit.semgus.chc)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:g #:com.kjcjohnson.synthkit.grammar)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:a #:alexandria)
                    (#:? #:trivia)))

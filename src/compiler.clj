(ns compiler
  (:require [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is]]))

;; want to write a function that emits something like this:

;;     .text
;;     .globl	scheme_entry
;;     .type	scheme_entry, @function
;; scheme_entry:
;;     pushq   %rbp
;;     movq	   %rsp, %rbp
;;     movl	   $39, %eax
;;     popq	   %rbp
;;     ret

(defn compile [program]
  (let [asm
        (with-out-str
          (println "\t.text")
          (println "\t.globl scheme_entry")
          (println "scheme_entry:")
          (println "\tpushq %rbp")
          (println "\tmovq %rsp, %rbp")
          (println (format "\tmovl $%d, %%eax" program))
          (println "\tpopq %rbp")
          (println "\tret"))]
    (spit "output.s" asm)))

(defn compile-and-run [program]
  (compile program)
  (let [{:keys [exit out err]} (sh/sh "make" "run")]
    (if (= exit 0)
      (:out (sh/sh "./output"))
      (str "Error during compilation: " err))))


(deftest emit-integers
  (is (= (compile-and-run 2) "2\n"))
  (is (= (compile-and-run true) "true\n"))
  )

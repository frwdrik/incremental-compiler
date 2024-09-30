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

(def fxmask 2r11)
(def fxtag 2r00)
(def bool-bit 6)
(def bool-f 2r00101111)

(defn immediate-rep [x]
  (cond
    (integer? x)
    (bit-shift-left x 2)

    (boolean? x)
    (if x
      2r01101111
      2r00101111)

    (char? x)
    (-> (int x)
        (bit-shift-left 8)
        (bit-or 2r00001111))
    
    (nil? x)
    2r111111))

(defn immediate? [x]
  (or (integer? x)
      (boolean? x)
      (char? x)
      (nil? x)))

(declare emit-expr)

(defn fxadd1 [x]
  (emit-expr x)
  (println (format "\taddl $%s, %%eax" (immediate-rep 1))))

(defn fxsub1 [x]
  (emit-expr x)
  (println (format "\tsubl $%s, %%eax" (immediate-rep 1))))

(defn fixnum? [x]
  (emit-expr x)
  (println (format "\tand $%s, %%al" fxmask))
  (println (format "\tcmp $%s, %%al" fxtag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn emit-immediate [x]
  (println (format "\tmovl $%d, %%eax" (immediate-rep x))))

(def prim-call
  {'fxadd1 {:args-count 1
            :emitter fxadd1}
   'fxsub1 {:args-count 1
            :emitter fxsub1}
   'fixnum? {:args-count 1
             :emitter fixnum?}})

(defn emit-prim-call [x args]
  (let [{:keys [args-count emitter]} (prim-call x)]
    (when-not (= args-count (count args))
      (ex-info (str "Wrong number of arguments to " x) {}))
    (apply emitter args)))

(defn emit-expr [x]
  (cond
    (immediate? x)
    (emit-immediate x)

    (and (list? x)
         (contains? prim-call (first x)))
    (emit-prim-call (first x) (rest x))))

(defn emit-function-header [function-header]
  (println "\t.text")
  (println (str "\t.globl " function-header))
  (println (str function-header ":")))

(defn emit-program [program]
  (let [asm
        (with-out-str
          (emit-function-header "scheme_entry")
          (emit-expr program)
          (println "\tret"))]
    (spit "output.s" asm)))

(defn compile-and-run [program]
  (emit-program program)
  (let [{:keys [exit _out err]} (sh/sh "make" "run")]
    (if (= exit 0)
      (:out (sh/sh "./output"))
      (str "Error during compilation: " err))))

(deftest emit-integers
  (is (= "2\n" (compile-and-run 2)))
  (is (= "-42\n" (compile-and-run -42))))

(deftest emit-booleans
  (is (= "true\n" (compile-and-run true)))
  (is (= "false\n" (compile-and-run false))))

(deftest emit-characters
  (is (= "#\\A\n" (compile-and-run \A)))
  (is (= "#\\a\n" (compile-and-run \a))))

(deftest emit-nil
  (is (= "()\n" (compile-and-run nil))))

(deftest immediate-rep-test
  (is (= 2r1100 (immediate-rep 3)))
  (is (= "11111111111111111111111111111100" (Integer/toBinaryString (immediate-rep -1))))
  (is (= 111 (immediate-rep true)))
  (is (= 0x6F (immediate-rep true)))
  (is (= 47 (immediate-rep false)))
  (is (= 0x2F (immediate-rep false))))

(deftest prim-call-test
  (is (= "2\n" (compile-and-run '(fxadd1 1))))
  (is (= "1\n" (compile-and-run '(fxsub1 2))))
  (is (= "1\n" (compile-and-run '(fxsub1 (fxadd1 1)))))
  (is (= "-1\n" (compile-and-run '(fxsub1 0))))
  (is (= "true\n" (compile-and-run '(fixnum? 3))))
  (is (= "false\n" (compile-and-run '(fixnum? true)))))

;; First, run the Clojure compiler
(compile-and-run true)
;; Then, run the makefile
;; $ make run

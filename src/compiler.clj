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

(def objectmask 2r111111)

(def booltag 2r101111)
(def bool-bit 6)
(def bool-t 2r01101111)
(def bool-f 2r00101111)

(def chartag 2r001111)

(def niltag 2r111111)

(defn immediate-rep [x]
  (cond
    (integer? x)
    (bit-shift-left x 2)

    (boolean? x)
    (if x
      bool-t
      bool-f)

    (char? x)
    (-> (int x)
        (bit-shift-left 8)
        (bit-or chartag))
    
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
;;=> Syntax error compiling at (src/compiler.clj:57:3).
;;   Unable to resolve symbol: emit-expr in this context
;;   

(defn bool? [x]
  (emit-expr x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" booltag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn character? [x]
  (emit-expr x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" chartag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn null? [x]
  (emit-expr x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" niltag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn fixnum->char [x]
  (emit-expr x)
  (println "\tsal $6, %eax")
  (println (format "\tor $%s, %%eax" chartag)))

(defn char->fixnum [x]
  (emit-expr x)
  (println "\tsar $6, %eax")
)

(defn emit-immediate [x]
  (println (format "\tmovl $%d, %%eax" (immediate-rep x))))

(def prim-call
  {'fxadd1 {:args-count 1
            :emitter fxadd1}
   'fxsub1 {:args-count 1
            :emitter fxsub1}
   'fixnum? {:args-count 1
             :emitter fixnum?}
   'bool? {:args-count 1
           :emitter bool?}
   'character? {:args-count 1 
                :emitter character?}
   'null? {:args-count 1
           :emitter null?}
   'fixnum->char {:args-count 1
                  :emitter fixnum->char}
   'char->fixnum {:args-count 1
                  :emitter char->fixnum }})

(defn emit-prim-call [x args]
  (let [{:keys [args-count emitter]} (prim-call x)]
    (when-not (= args-count (count args))
      (ex-info (str "Wrong number of arguments to " x) {}))
    (apply emitter args)))

(let [n (volatile! -1)]
 (defn unique-label []
   (format "L.%s" (vswap! n inc))))

(defn if? [x]
  (and (seq? x)
       (= 'if (first x))
       (= (count x) 4)))

;; 	.text
;; 	.globl	main
;; main:
;; 	movl	$0, -4(%rbp) 
;; 	cmpl	$0, -4(%rbp)
;; 	je	.L2
;; 	movl	$3, %eax
;; 	jmp	.L3
;; .L2:
;; 	movl	$5, %eax
;; .L3:
;; 	ret

;; (test) -- then --  L3
;;      \             /
;;       \           /
;;         L2: else   


(defn emit-if [[_if test then else]]
  ;; 0: create labels
  ;; 1: emit test
  ;; 2: do a compare
  ;; 3: jump to right places based on compare
  ;; 4: emit code for then and else brances in correct places
  (let [altern-label (unique-label)
        end-label (unique-label)]
    (emit-expr test)
    (println (format "\tcmp $%s, %%eax" bool-f))
    ;; 
    ))

(defn emit-expr [x]
  (cond
    (immediate? x)
    (emit-immediate x)

    (and (list? x)
         (contains? prim-call (first x)))
    (emit-prim-call (first x) (rest x))

    (if? x)
    (emit-if x)))

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
  (is (= 0x2F (immediate-rep false)))
  (is (= "#\\a\n" (compile-and-run \a)))
  (is (= "#\\A\n" (compile-and-run \A))))

(deftest prim-call-test
  (is (= "2\n" (compile-and-run '(fxadd1 1))))
  (is (= "1\n" (compile-and-run '(fxsub1 2))))
  (is (= "1\n" (compile-and-run '(fxsub1 (fxadd1 1)))))
  (is (= "-1\n" (compile-and-run '(fxsub1 0))))

  (is (= "true\n" (compile-and-run '(fixnum? 3))))
  (is (= "false\n" (compile-and-run '(fixnum? true))))
  (is (= "true\n" (compile-and-run '(bool? true))))
  (is (= "true\n" (compile-and-run '(bool? false))))
  (is (= "false\n" (compile-and-run '(bool? 2))))
  (is (= "false\n" (compile-and-run '(character? 2))))
  (is (= "true\n" (compile-and-run '(character? \A))))
  (is (= "false\n" (compile-and-run '(null? 2))))
  (is (= "true\n" (compile-and-run '(null? nil))))

  (is (= "#\\a\n" (compile-and-run '(fixnum->char 97))))
  (is (= "#\\A\n" (compile-and-run '(fixnum->char 65))))
  
  (is (= "65\n" (compile-and-run '(char->fixnum (fixnum->char 65))))))

(deftest if-expr
  (is (= "true\n" (compile-and-run '(if false false true))))
  (is (= "true\n" (compile-and-run '(if true true false))))
  (is (= "true\n" (compile-and-run '(if (bool? false) true false))))
  )

;; First, run the Clojure compiler
(compile-and-run true)
;; Then, run the makefile
;; $ make run

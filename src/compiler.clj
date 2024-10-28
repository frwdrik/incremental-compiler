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

(defn fxadd1 [si env x]
  (emit-expr si env x)
  (println (format "\taddl $%s, %%eax" (immediate-rep 1))))

(defn fxsub1 [si env x]
  (emit-expr si env x)
  (println (format "\tsubl $%s, %%eax" (immediate-rep 1))))

(defn fixnum? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" fxmask))
  (println (format "\tcmp $%s, %%al" fxtag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))
;;=> Syntax error compiling at (src/compiler.clj:57:3).
;;   Unable to resolve symbol: emit-expr in this context
;;   

(defn bool? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" booltag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn character? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" chartag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn null? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" niltag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn fixnum->char [si env x]
  (emit-expr si env x)
  (println "\tsal $6, %eax")
  (println (format "\tor $%s, %%eax" chartag)))

(defn char->fixnum [si env x]
  (emit-expr si env x)
  (println "\tsar $6, %eax")
)

(defn fx+ [si env x y]
  (emit-expr si env x)
  (println (format "\tmovl %%eax, %s(%%rsp)" si))
  (emit-expr (- si 4) env y)
  (println (format "\taddl %s(%%rsp), %%eax" si)))

(defn fx- [si env x y]
  (emit-expr si env y)
  (println (format "\tmovl %%eax, %s(%%rsp)" si))
  (emit-expr (- si 4) env x)
  (println (format "\tsubl %s(%%rsp), %%eax" si)))

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
                  :emitter char->fixnum }
   'fx+ {:args-count 2
         :emitter fx+}
   'fx- {:args-count 2
         :emitter fx-}})

(defn emit-prim-call [si env x args]
  (let [{:keys [args-count emitter]} (prim-call x)]
    (when-not (= args-count (count args))
      (ex-info (str "Wrong number of arguments to " x) {}))
    (apply emitter si env args)))

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


(defn emit-if [si env [_if test then else]]
  ;; 0: create labels
  ;; 1: emit test
  ;; 2: do a compare
  ;; 3: jump to right places based on compare
  ;; 4: emit code for then and else brances in correct places
  (let [altern-label (unique-label)
        end-label (unique-label)]
    (emit-expr si env test)
    (println (format "\tcmp $%s, %%eax" bool-f))
    (println (format "\tje %s" altern-label))
    (emit-expr si env then)
    (println (format "\tjmp %s" end-label))
    (println (format "%s:" altern-label))
    (emit-expr si env else)
    (println (format "%s:" end-label))))

(defn let? [x]
  (and (seq? x)
       (= 'let (first x))))

(defn emit-let [si env [_let bindings & body]]
  ;; Initialize
  ;;   si <- si
  ;;   env <- env
  ;; While there are bindings (var, expr):
  ;;   1. emit expr
  ;;   2. move expr into stack at stack index
  ;;   3. env <- (assoc env var si)
  ;;   4. si <- si-4
  ;; Emit body using si and env
  (loop [si si
         env env
         bindings bindings]
    (if (empty? bindings)
      (emit-expr si env (cons 'do body))
      ;; body = ((println 1) 3)
      ;; (cons 'do body) = (do (println 1) 3)
      ;; (let [x 1]
      ;;   (println 1)
      ;;   3)
      (do
        (emit-expr si env (second bindings))
        (println (format "\tmov %%eax, %s(%%rsp)" si))
        (recur (- si 4)
               (assoc env (first bindings) si)
               (drop 2 bindings))))))

'(let [x 1]
   x)

;; x is a local variable.
;; We are storing local variables on the stack.
;; Hence, x needs to be assigned a stack index.
;; 
;; Our compiler needs to keep track of all stack index assignments,
;; because when we evaluate the body of a let, we might encounter variable names,
;; and those should be compiled into mov's from the corresponding stack index.

;; mov $8, %eax

(defn do? [x]
  (and (seq? x)
       (= 'do (first x))))

(defn emit-do [si env [_do & body]]
  (mapv #(emit-expr si env %) body))

(defn emit-expr [si env x]
  (cond
    (immediate? x)
    (emit-immediate x)

    (and (list? x)
         (contains? prim-call (first x)))
    (emit-prim-call si env (first x) (rest x))

    (if? x)
    (emit-if si env x)

    (let? x)
    (emit-let si env x)
    
    (do? x)
    (emit-do si env x)))

(defn emit-function-header [function-header]
  (println "\t.text")
  (println (str "\t.globl " function-header))
  (println (str function-header ":")))

(def empty-env {})

(defn emit-program [program]
  (let [asm
        (with-out-str
          (emit-function-header "scheme_entry")
          (println "\tmov %rsp, %rcx")
          (println "\tmov %rdi, %rsp")
          (emit-expr -4 empty-env program)
          (println "\tmov %rcx, %rsp")
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
  (is (= "5\n" (compile-and-run '(if false 3 5))))
  (is (= "3\n" (compile-and-run '(if true 3 5)))))

(deftest binary-prim-call-test
  (is (= "5\n" (compile-and-run '(fx+ 2 3))))
  (is (= "10\n" (compile-and-run '(fx+ 2 (fx+ 3 5)))))
  (is (= "2\n" (compile-and-run '(fx- 5 3))))
  (is (= "-2\n" (compile-and-run '(fx- 3 5))))
  (is (= "12\n" (compile-and-run '(fx- 10 (fx- 3 5)))))
  (is (= "2\n" (compile-and-run '(fx- 10 (fx+ 3 5))))))

(deftest let-expr
  (is (= "1\n" (compile-and-run '(let [x 1] x)))))

(deftest do-expr
  (is (= "1\n" (compile-and-run '(do 1)))))


;; First, run the Clojure compiler
(compile-and-run true)
;; Then, run the makefile
;; $ make run

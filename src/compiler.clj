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

(def boolmask 2r111111)

(def booltag 2r101111)
(def bool-bit 6)
(def bool-t 2r01101111)
(def bool-f 2r00101111)

(def charmask 2r111111)
(def chartag 2r001111)

(def nilmask 2r111111)
(def niltag 2r111111)

(def objectmask 2r111)
(def pairtag 2r001)

(defn emit-stack-save [si]
  (println (format "\tmov %%rax, %s(%%rsp)" si)))

(defn emit-stack-load [si]
  (println (format "\tmov %s(%%rsp), %%rax" si)))

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
(declare emit-tail-expr)

(defn emit-heap-alloc [size]
  ;; rbp contains the pointer to the next available byte in memory
  ;; At the end of the function, the heap pointer will be incremented by size
  ;; Will return the starting address in %rax
  (println "\tmov %rbp, %rax")
  (println (format "\tadd $%s, %%rbp" size)))

(defn emit-cons [si env x y]
  (emit-expr si env x) ;; emit x
  (emit-stack-save si)
  (emit-expr (- si 8) env y) ;; emit y
  (emit-stack-save (- si 8))
  (emit-heap-alloc 16)
  (println (format "\tmov %s(%%rsp), %%rdx" si))               ;; move x into rdx register
  (println "\tmov %rdx, (%rax)")                               ;; move rdx register into allocated memory
  (println (format "\tmov %s(%%rsp), %%rdx" (- si 8)))         ;; move y into rdx register
  (println "\tmov %rdx, 8(%rax)")                              ;; move rdx register into allocated memory
  (println (format "\tor $%s, %%al" pairtag)))                 ;; tag pointer

(defn emit-car [si env x]
  ;; Emit x (the pair)
  ;; Subtract pair tag
  ;; (8b (first), 8b (second))
  ;; Removing tag from address of cons cell to get address of first element
  (emit-expr si env x)
  (println "\tmov -1(%rax), %rax"))

(defn emit-cdr [si env x]
  ;; Emit x (the pair)
  ;; Subtract pair tag
  ;; (8b (first), 8b (second))
  ;; Removing tag from address of cons cell to get address of first element,
  ;; Move 8b up to second element
  (emit-expr si env x)
  (println "\tmov 7(%rax), %rax"))

(defn fxadd1 [si env x]
  (emit-expr si env x)
  (println (format "\taddq $%s, %%rax" (immediate-rep 1))))

(defn fxsub1 [si env x]
  (emit-expr si env x)
  (println (format "\tsubq $%s, %%rax" (immediate-rep 1))))

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
  (println (format "\tand $%s, %%al" boolmask))
  (println (format "\tcmp $%s, %%al" booltag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn character? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" charmask))
  (println (format "\tcmp $%s, %%al" chartag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn null? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" nilmask))
  (println (format "\tcmp $%s, %%al" niltag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn pair? [si env x]
  (emit-expr si env x)
  (println (format "\tand $%s, %%al" objectmask))
  (println (format "\tcmp $%s, %%al" pairtag))
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn fixnum->char [si env x]
  (emit-expr si env x)
  (println "\tsal $6, %eax")
  (println (format "\tor $%s, %%eax" chartag)))

(defn emit-not [si env x]
  (emit-expr si env x)
  (println (format "\tcmp $%s, %%al" bool-f))
  ;; return #t if EXPR is false, returns #f otherwise
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn char->fixnum [si env x]
  (emit-expr si env x)
  (println "\tsar $6, %eax")
)

(defn fx+ [si env x y]
  (emit-expr si env x)
  (println (format "\tmovq %%rax, %s(%%rsp)" si))
  (emit-expr (- si 8) env y)
  (println (format "\taddq %s(%%rsp), %%rax" si)))

(defn fx- [si env x y]
  (emit-expr si env y)
  (println (format "\tmovq %%rax, %s(%%rsp)" si))
  (emit-expr (- si 8) env x)
  (println (format "\tsubq %s(%%rsp), %%rax" si)))

(defn fxzero? [si env x]
  (emit-expr si env x)
  (println "\ttest %rax, %rax")
  (println "\tsete %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn leq [si env x y]
  (emit-expr si env x)
  (emit-stack-save si)
  (emit-expr (- si 8) env y)
  (println (format "\tcmp %%rax, %s(%%rsp)" si))
  (println "\tsetle %al")
  (println "\tmovzbl %al, %eax")
  (println (format "\tsal $%s, %%al" bool-bit))
  (println (format "\tor $%s, %%al" bool-f)))

(defn emit-immediate [x]
  (println (format "\tmovq $%d, %%rax" (immediate-rep x))))

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
   'pair? {:args-count 1
           :emitter pair?}
   'not {:args-count 1
         :emitter emit-not}
   'fixnum->char {:args-count 1
                  :emitter fixnum->char}
   'char->fixnum {:args-count 1
                  :emitter char->fixnum }
   '+ {:args-count 2
       :emitter fx+}
   '- {:args-count 2
       :emitter fx-}
   'zero? {:args-count 1
           :emitter fxzero?}
   'cons {:args-count 2
         :emitter emit-cons}
   'cdr {:args-count 1
         :emitter emit-cdr}
   'car {:args-count 1
         :emitter emit-car}
   'leq {:args-count 2
           :emitter leq}})

(defn emit-prim-call [si env x args]
  (let [{:keys [args-count emitter]} (prim-call x)]
    (when-not (= args-count (count args))
      (ex-info (str "Wrong number of arguments to " x) {}))
    (apply emitter si env args)))

(let [n (volatile! -1)]
  (defn unique-label [prefix]
    (format "%s.%s" prefix (vswap! n inc))))

(defn if? [x]
  (and (seq? x)
       (= 'if (first x))
       (= (count x) 4)))

(defn letfn? [x]
  (and (seq? x)
       (= 'letfn (first x))))

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
  (let [altern-label (unique-label "altern")
        end-label (unique-label "end")]
    (emit-expr si env test)
    (println (format "\tcmp $%s, %%eax" bool-f))
    (println (format "\tje %s" altern-label))
    (emit-expr si env then)
    (println (format "\tjmp %s" end-label))
    (println (format "%s:" altern-label))
    (emit-expr si env else)
    (println (format "%s:" end-label))))

(defn emit-tail-if [si env [_if test then else]]
  (let [altern-label (unique-label "altern")
        end-label (unique-label "end")]
    (emit-expr si env test)
    (println (format "\tcmp $%s, %%eax" bool-f))
    (println (format "\tje %s" altern-label))
    (emit-tail-expr si env then)
    (println (format "\tjmp %s" end-label))
    (println (format "%s:" altern-label))
    (emit-tail-expr si env else)
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
  ;;   4. si <- si-8
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
        (println (format "\tmov %%rax, %s(%%rsp)" si))
        (recur (- si 8)
               (assoc env (first bindings) si)
               (drop 2 bindings))))))

(defn emit-tail-let [si env [_let bindings & body]]
  (loop [si si
         env env
         bindings bindings]
    (if (empty? bindings)
      (emit-tail-expr si env (cons 'do body))
      (do
        (emit-expr si env (second bindings))
        (println (format "\tmov %%rax, %s(%%rsp)" si))
        (recur (- si 8)
               (assoc env (first bindings) si)
               (drop 2 bindings))))))

(defn emit-function-header [function-header]
  (println "\t.text")
  (println (str "\t.globl " function-header))
  (println (str function-header ":")))

(defn emit-ret []
  (println "\tret"))

(defn emit-scheme-entry [program env]
  (emit-function-header "__scheme_entry")
  (emit-tail-expr -8 env program)
  (emit-ret))

(defn emit-push [x]
  (println (format "\tpush %s" x)))

(defn emit-call [label]
  (println (format "\tcall %s" label)))

(defn emit-adjust-stack [offset]
  (println (format "\tadd $%d, %%rsp" offset)))

(declare emit-expr)

(defn emit-app [si env [fn-name & args]]
  ;; si = -100  (random numbers)
  ;; rsp = 1000
  ;;
  ;; STACK FRAME BEFORE CALL
  ;; stack address        stack content
  ;; 992                  ~~~~~
  ;; 1000 (rsp)           ~~~~~
  ;; ...
  ;;
  ;; 900                  <free space for emit-app>
  ;; x (=900)             return address
  ;; 892                  arg3
  ;; 884                  arg2
  ;;
  ;;
  ;; STACK FRAME DIRECTLY AFTER CALL
  ;; stack address        stack content
  ;; 900 (rsp)            return-address
  ;; 900-8                  arg3
  ;; 900-16                 arg2
  ;; ...                  ...
  ;;

  ;; Start emitting args at `(- si 8)`, so as to leave room for "call"
  ;; to push the return address at `si`.
  (loop [[arg & rargs] args
         si (- si 8)]
    (when arg
      (emit-expr si env arg)
      (emit-stack-save si)
      (recur rargs (- si 8))))
  (emit-adjust-stack (+ si 8)) ;; rsp = 908
  (emit-call fn-name)          ;; 1. decrement rsp to 900, 2. save return address at memory pointed to by rsp
  (emit-adjust-stack (- (+ si 8))))

(defn emit-tail-app [si env [fn-name & args]]
  (loop [[arg & rargs] args
         si si]
    (when arg
      (emit-expr si env arg)
      (emit-stack-save si)
      (recur rargs (- si 8))))
  ;; Offset = (- (rsp - 8) si)
  ;; For each arg, move arg by offset
  (let [offset (- (- si) 8)]
    (loop [[arg & rest] args
           si si]
      (when arg
        (emit-stack-load si)
        (emit-stack-save (+ si offset))
        (recur rest (- si 8)))))
  (println (str "\tjmp " fn-name)))

(defn emit-lambda [env label [args & body]]
  (emit-function-header label)
  (let [env (merge env
                   (zipmap args (iterate #(- % 8) (- 8))))]
    (emit-tail-expr (* (inc (count args)) -8) env (cons 'do body))))

(defn emit-letfn [[_letfn bindings & body]]
  (let [lvars (map first bindings)
        labels lvars
        lambdas (map rest bindings)
        env (zipmap lvars labels)]
    (mapv (partial emit-lambda env) labels lambdas)
    (emit-scheme-entry (cons 'do body) env)))

;; '(let [x 1]
;;    x)

;; x is a local variable.
;; We are storing local variables on the stack.
;; Hence, x needs to be assigned a stack index.
;; 
;; Our compiler needs to keep track of all stack index assignments,
;; because when we evaluate the body of a let, we might encounter variable names,
;; and those should be compiled into mov's from the corresponding stack index.

(defn do? [x]
  (and (seq? x)
       (= 'do (first x))))

(defn emit-do [si env [_do & body]]
  (mapv #(emit-expr si env %) body))

(defn emit-tail-do [si env [_do & body]]
  (mapv #(emit-expr si env %) (butlast body))
  (emit-tail-expr si env (last body)))

(defn variable? [x]
  (symbol? x))

(defn emit-variable [env var]
  (if-let [si (get env var)]
    (println (format "\tmov %s(%%rsp), %%rax" si))
    (throw (ex-info "Unknown local" {:local var :env env}))))

(defn emit-expr [si env x]
  (cond
    (immediate? x)
    (emit-immediate x)

    (and (list? x)
         (contains? prim-call (first x)))
    (emit-prim-call si env (first x) (rest x))

    (and (list? x)
         (contains? env (first x)))
    (emit-app si env x)

    (if? x)
    (emit-if si env x)

    (let? x)
    (emit-let si env x)
    
    (variable? x)
    (emit-variable env x)
    
    (do? x)
    (emit-do si env x)

    (letfn? x)
    (emit-letfn x)))

(defn emit-tail-expr [si env x]
  (cond
    (immediate? x)
    (do (emit-immediate x)
        (emit-ret))

    (and (list? x)
         (contains? prim-call (first x)))
    (do (emit-prim-call si env (first x) (rest x))
        (emit-ret))

    (and (list? x)
         (contains? env (first x)))
    (emit-tail-app si env x)

    (if? x)
    (emit-tail-if si env x)

    (let? x)
    (emit-tail-let si env x)

    (variable? x)
    (do (emit-variable env x)
        (emit-ret))

    (do? x)
    (emit-tail-do si env x)))

(def empty-env {})

(defn emit-program [program]
  ;; address of ctxt: rdi
  ;; stack base: rsi
  ;; heap: rdx
  ;; We want to store the current state of registers in the context
  ;;  rax; /* 0 scratch */
  ;;  rbx; /* 8 preserve */
  ;;  rcx; /* 16 scratch */
  ;;  rdx; /* 24 scratch */
  ;;  rsi; /* 32 preserve */
  ;;  rdi; /* 40 preserve */
  ;;  rbp; /* 48 preserve */
  ;;  rsp; /* 56 preserve */
  (let [asm
        (with-out-str
          (emit-function-header "scheme_entry")
          ;; Storing context address
          (println "\tmov %rdi, %rcx")
          ;; Saving register values in context
          (println "\tmov %rbx,  8(%rcx)")
          (println "\tmov %rsi, 32(%rcx)")
          (println "\tmov %rdi, 40(%rcx)")
          (println "\tmov %rbp, 48(%rcx)")
          (println "\tmov %rsp, 56(%rcx)")
          ;; Set the stack pointer to stack top
          (println "\tmov %rsi, %rsp")
          ;; Set the heap pointer
          (println "\tmov %rdx, %rbp")
          ;; Begin program
          (println "\tcall __scheme_entry")
          ;; Restore pointers
          (println "\tmov 8(%rcx), %rbx")
          (println "\tmov 32(%rcx), %rsi")
          (println "\tmov 40(%rcx), %rdi")
          (println "\tmov 48(%rcx), %rbp")
          (println "\tmov 56(%rcx), %rsp")

          (println "\tret")
          (if (letfn? program)
            (emit-letfn program)
            (emit-scheme-entry program empty-env)))]
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
  
  (is (= "65\n" (compile-and-run '(char->fixnum (fixnum->char 65)))))

  (is (= "true\n" (compile-and-run '(zero? 0))))
  (is (= "false\n" (compile-and-run '(zero? 1))))
  (is (= "false\n" (compile-and-run '(zero? (+ 1 0)))))
  (is (= "true\n" (compile-and-run '(zero? (- 99 (+ 90 9)))))))

(deftest if-expr
  (is (= "true\n" (compile-and-run '(if false false true))))
  (is (= "true\n" (compile-and-run '(if true true false))))
  (is (= "true\n" (compile-and-run '(if (bool? false) true false))))
  (is (= "5\n" (compile-and-run '(if false 3 5))))
  (is (= "3\n" (compile-and-run '(if true 3 5)))))

(deftest binary-prim-call-test
  (is (= "5\n" (compile-and-run '(+ 2 3))))
  (is (= "10\n" (compile-and-run '(+ 2 (+ 3 5)))))
  (is (= "2\n" (compile-and-run '(- 5 3))))
  (is (= "-2\n" (compile-and-run '(- 3 5))))
  (is (= "12\n" (compile-and-run '(- 10 (- 3 5)))))
  (is (= "2\n" (compile-and-run '(- 10 (+ 3 5))))))

;; In our "scheme" language, we can define and apply our own functions
;; like this:
;; (letfn [add-1 (fn (x)
;;                        (+ 1 x))]
;;         ;; body
;;         (add-1 3))

;; The similar code in JS would look like:
;; { const add1 = (x) => x+1;
;;   return add1(3); }

(deftest letfn-test
  (is (= "12\n" (compile-and-run '(letfn [] 12))))
  (is (= "10\n" (compile-and-run '(letfn [] (let [x 5] (+ x x))))))
  (is (= "7\n" (compile-and-run  '(letfn [(f () 5)] 7))))
  (is (= "12\n" (compile-and-run '(letfn [(f () 5)] (let [x 12] x)))))
  (is (= "5\n" (compile-and-run  '(letfn [(f () 5)] (f)))))
  (is (= "5\n" (compile-and-run  '(letfn [(f () 5)] (let [x (f)] x)))))
  (is (= "11\n" (compile-and-run '(letfn [(f () 5)] (+ (f) 6)))))
  (is (= "15\n" (compile-and-run '(letfn [(f () 5)] (- 20 (f))))))
  (is (= "10\n" (compile-and-run '(letfn [(f () 5)] (+ (f) (f))))))
  (is (= "-9\n" (compile-and-run '(letfn [(f (x) (- x 10))]
                                    (let [x 1]
                                      (f x))))))
  (is (= "-1\n" (compile-and-run '(letfn [(f (x y) (- x y))]
                                    (f 2 3)))))
  ;; Suggestion: Support mutually recursive functions
  #_(is (= "-9\n"
           (compile-and-run
            '(letfn [(f (x) (if (bool? x) 123 (g false)))
                     (g (y) (f y))]
               ;; This call should go like
               ;;   - (g 1)
               ;;   - (f 1)
               ;;   - (g false)
               ;;   - (f false) => done, return 123
               (g 1)))))
  )

(deftest emit-tail-expr-test
  (is (= "true\n" (compile-and-run '(letfn [(f (x)
                                                       (if (zero? x)
                                                         true
                                                         false))]
                                            (f 0)))))
  (is (= "9\n" (compile-and-run '(letfn [(f (x)
                                                    (if (zero? x)
                                                      9
                                                      (f (- x 1))))]
                                         (f 3))))))
(deftest let-expr
  (is (= "1\n" (compile-and-run '(let [x 1] x))))
  (is (= "-5\n" (compile-and-run '(let [x 1] (+ x 3) (- x 6)))))
  (is (= "-5\n" (compile-and-run '(let [x 1] (- x 6))))))

(deftest do-expr
  (is (= "1\n" (compile-and-run '(do 1)))))

(deftest not-expr
  (is (= "false\n" (compile-and-run '(not true))))
  (is (= "true\n" (compile-and-run '(not false))))
  (is (= "false\n" (compile-and-run '(not 1)))))

(deftest cons-expr
  (is (= "false\n" (compile-and-run '(null? (cons 1 2)))))
  (is (= "true\n" (compile-and-run '(pair? (cons 1 2)))))
  (is (= "true\n" (compile-and-run '(pair? (cons 10 (cons 1 2))))))
  (is (= "false\n" (compile-and-run '(not (cons 1 2)))))
  
  (is (= "1\n" (compile-and-run '(car (cons 1 2)))))
  (is (= "2\n" (compile-and-run '(cdr (cons 1 2))))) 
  (is (= "5\n" (compile-and-run '(cdr (car (cons (cons 1 5) 3))))))
  (is (= "(10 2)\n" (compile-and-run '(cons 10 2))))
  (is (= "(10 (1 2))\n" (compile-and-run '(cons 10 (cons 1 2)))))
  (is (= "(10 ((1 3) 2))\n" (compile-and-run '(cons 10 (cons (cons 1 3) 2))))))

(deftest less-than-or-equal-test
  (is (= "true\n" (compile-and-run '(leq 1 2))))
  (is (= "true\n" (compile-and-run '(leq 1 1))))
  (is (= "false\n" (compile-and-run '(leq 5 2)))))


;; 0 1 1 2 3 5 8 13 21 34 55
;; 0 1 2 3 4 5 6 7  8  9  10
(deftest fibonacci
  (is (= "55\n" (compile-and-run
                 '(letfn [(fibonacci (n)
                            (if (leq n 1)
                              n
                              (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))]
                    (fibonacci 10)))))
  (is (= "55\n" (compile-and-run
                 '(letfn [(fibonacci (fib1 fib2 counter)
                            (if (leq counter 0)
                              fib1
                              (fibonacci fib2 (+ fib1 fib2) (- counter 1))))]
                    (fibonacci 0 1 10))))))

(compile-and-run
                 '(letfn [(fibonacci (fib1 fib2 counter)
                            (if (leq counter 0)
                              fib1
                              (fibonacci fib2 (+ fib1 fib2) (- counter 1))))]
                    (fibonacci 0 1 100)))
 ;;         div
 ;;       /     \
 ;;      p      "d"   
 ;;   /  |  \     
 ;; "a"  b  "c"
 ;;      |   
 ;;     "x" 

;; NODE: LEAF | INTERNAL
;; LEAF: (cons <char> nil)
;; INTERNAL: (cons <char> NODES)
;; NODES: (cons NODE NODES) | (cons NODE nil)
(compile-and-run '(let [x (cons \x nil)
        b (cons \b (cons x nil))
        a (cons \a nil)
        c (cons \c nil)
        p (cons \p (cons a (cons b (cons c nil))))
        d (cons \d nil)
        div (cons \D (cons p (cons d nil)))]
    div))

(compile-and-run '(let [x (cons nil nil)] x))

(compile-and-run '(let [a 1]
                    (cons a nil)))

(compile-and-run '(let [a 1]
                    (cons a nil)))

(compile-and-run '(let [a (cons 1 1)]
                    (cons 1 1)))

(compile-and-run '(let [a (cons 1 1)]
                    a))

(compile-and-run '(cons \a (cons \x (cons \c (cons \d nil)))))

(compile-and-run '(letfn))

;; First, run the Clojure compiler
;;     (compile-and-run true)
;; Then, run the makefile
;;     $ make run


;; 0xf7f9d001
;; 0x7ffff

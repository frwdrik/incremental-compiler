(ns compiler)

(defn emit-program [num]
  (spit "program.s"
        (str "\t.text\n"
             "\t.globl _scheme_entry\n"
             "_scheme_entry:\n"
             "\tpushq %rbp\n"
             "\tmovq %rsp, %rbp\n"
             "\tmovl $" num ", %eax\n"
             "\tpopq %rbp\n"
             "\tret\n")))

(emit-program 42.01)

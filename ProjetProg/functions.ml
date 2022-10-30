open X86_64

(* Assembly code to compute powers *)
let text_pow =
  label "power" ++
  movq (imm 1) (reg rax) ++
  jmp "while_pow" ++
  label "while_pow" ++
  cmpq (imm 1) (reg rsi) ++
  js "end_pow" ++
  decq (reg rsi) ++
  imulq (reg rdi) (reg rax) ++
  jmp "while_pow" ++
  label "end_pow" ++
  ret

(* Assembly code to compute factorial *)
let text_fact =
  label "factorial" ++
  movq (imm 1) (reg rax) ++
  jmp "while_fact" ++
  label "while_fact" ++
  orq (imm 0) (reg rdi) ++
  je "end_fact" ++
  imulq (reg rdi) (reg rax) ++
  decq (reg rdi) ++
  jmp "while_fact" ++
  label "end_fact" ++
  ret

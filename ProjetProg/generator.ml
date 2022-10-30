open Asyntax
open X86_64
open Typing

let generate_code exp =
	(* Boolean variable used to know which functions have to be used in the assembly code *)
        let have_pow = ref false in
        let have_fact = ref false in
	
	(* Array containing one element for each variable, that is NotDef if it is not used, else its type *)
        let variables = Array.make 26 NotDef in

	(* Writing of the assembly code with the X86_64 OCaml module *)
	(* i_float is the number of float constants already defined, used to index them *)
        let rec text_code exp i_float =
                match exp with
                |Aff (c, e, suite) -> let type_var = find_type_exp ~variables:variables e in
                                      variables.((Char.code c) - 97) <- type_var;
                                      let (t, d, i) = text_code e i_float in
                                      let (ts, ds, is) = text_code suite i in
                                      let name = "."^(String.make 1 c) in
                                      let ini_var = if type_var = ExpInt then dquad [0] else double 0. in
                                      let reg_var = if type_var = ExpInt then r15 else xmm0 in
                                      (t ++
                                       inline "\n" ++
                                       leaq (reg (name^"(%rip)")) r13 ++
                                       movq (reg reg_var) (ind r13) ++
                                       inline "\n" ++
                                       ts,
                                       d ++
                                       label name ++
                                       ini_var ++
                                       ds,
                                       is)
                |Int x -> (movq (imm x) (reg r15), inline "", i_float)
                |Float x -> let d = ".FC" ^ (string_of_int i_float) in
                        (movsd (flt (d^"(%rip)")) (reg xmm0),
                        label d ++
                        double x,
                        i_float + 1)
                |Var c -> let name = "."^(String.make 1 c) in
                          let type_var = variables.((Char.code c) - 97) in
                          let reg_var = if type_var = ExpInt then r15 else xmm0 in
                          (movq (reg (name^"(%rip)")) (reg reg_var),
                           inline "",
                           i_float)
                |FloatPlus (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                movsd (reg xmm0) (ind ~ofs:(-8) rsp) ++
                                subq (imm 8) (reg rsp) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                movsd (ind rsp) (reg xmm1) ++ 
                                addq (imm 8) (reg rsp) ++
                                addsd (reg xmm1) (reg xmm0),
                                d1 ++
                                d2,
                                i2)
                |Plus (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                pushq (reg r15) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                popq (r14) ++
                                addq (reg r14) (reg r15),
                                d1 ++
                                d2,
                                i2)
                |FloatMinus (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                        let (t2, d2, i2) = (text_code e2 i1) in
                                        (t1 ++
                                        inline "\n" ++
                                        movsd (reg xmm0) (ind ~ofs:(-8) rsp) ++
                                        subq (imm 8) (reg rsp) ++
                                        inline "\n" ++
                                        t2 ++
                                        inline "\n" ++
                                        movsd (ind rsp) (reg xmm1) ++
                                        addq (imm 8) (reg rsp) ++
                                        subsd (reg xmm0) (reg xmm1) ++
                                        movsd (reg xmm1) (reg xmm0),
                                        d1 ++
                                        d2,
                                        i2)
                |Minus (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                pushq (reg r15) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                popq (r14) ++
                                subq (reg r15) (reg r14) ++
                                movq (reg r14) (reg r15),
                                d1 ++
                                d2,
                                i2)
                |FloatTimes (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                        let (t2, d2, i2) = (text_code e2 i1) in
                                        (t1 ++
                                        inline "\n" ++
                                        movsd (reg xmm0) (ind ~ofs:(-8) rsp) ++
                                        subq (imm 8) (reg rsp) ++
                                        inline "\n" ++
                                        t2 ++
                                        inline "\n" ++
                                        movsd (ind rsp) (reg xmm1) ++
                                        addq (imm 8) (reg rsp) ++
                                        mulsd (reg xmm1) (reg xmm0),
                                        d1 ++
                                        d2,
                                        i2)
                |Times (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                pushq (reg r15) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                popq (r14) ++
                                imulq (reg r14) (reg r15),
                                d1 ++
                                d2,
                                i2)
                |FloatDiv (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                movsd (reg xmm0) (ind ~ofs:(-8) rsp) ++
                                subq (imm 8) (reg rsp) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                movsd (ind rsp) (reg xmm1) ++
                                addq (imm 8) (reg rsp) ++
                                divsd (reg xmm0) (reg xmm1) ++
                                movsd (reg xmm1) (reg xmm0),
                                d1 ++
                                d2,
                                i2)
                |Div (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                pushq (reg r15) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                popq (rax) ++
                                xorq (reg rdx) (reg rdx) ++
                                cqto ++
                                idivq (reg r15) ++
                                movq (reg rax) (reg r15),
                                d1 ++
                                d2,
                                i2)
                |Mod (e1, e2) -> let (t1, d1, i1) = (text_code e1 i_float) in
                                let (t2, d2, i2) = (text_code e2 i1) in
                                (t1 ++
                                inline "\n" ++
                                pushq (reg r15) ++
                                inline "\n" ++
                                t2 ++
                                inline "\n" ++
                                popq rax ++
                                xorq (reg rdx) (reg rdx) ++
                                idivq (reg r15) ++
                                movq (reg rdx) (reg r15),
                                d1 ++
                                d2,
                                i2)
                |Pow (e1, e2) -> have_pow := true;
                                 let (t1, d1, i1) = (text_code e1 i_float) in
                                 let (t2, d2, i2) = (text_code e2 i1) in
                                 (t1 ++
                                 inline "\n" ++
                                 pushq (reg r15) ++
                                 inline "\n" ++
                                 t2 ++
                                 inline "\n" ++
                                 popq rdi ++
                                 movq (reg r15) (reg rsi) ++
                                 call "power" ++
                                 movq (reg rax) (reg r15),
                                 d1 ++
                                 d2,
                                 i2)
                |Fact e -> have_fact := true;
                           let (t, d, i) = text_code e i_float in
                           (t ++
                           inline "\n" ++
                           movq (reg r15) (reg rdi) ++
                           call "factorial" ++
                           movq (reg rax) (reg r15),
                           d,
                           i)
                |Pos e -> text_code e i_float
                |Neg e -> let (t, d, i) = text_code e i_float in
                        (t ++
                        inline "\n" ++
                        negq (reg r15) ++
                        xorpd (reg xmm1) (reg xmm1) ++
                        subsd (reg xmm0) (reg xmm1) ++
                        movsd (reg xmm1) (reg xmm0),
                        d,
                        i)
                |OpInt e -> let (t, d, i) = text_code e i_float in
                        (t ++ 
                        cvttsd2siq (reg xmm0) (reg r15),
                        d,
                        i)
                |OpFloat e -> let (t, d, i) = text_code e i_float in
                        (t ++ 
                        cvtsi2sdq (reg r15) (reg xmm0),
                        d,
                        i)

        in let (t, d, i) = text_code exp 0 in
	(* Potential additional functions to write *)
        let t_fun = ref (inline "\n") in
        if (!have_pow) then t_fun:= (!t_fun) ++ Functions.text_pow ++ inline "\n" else ();
        if (!have_fact) then t_fun:= (!t_fun) ++ Functions.text_fact ++ inline "\n" else ();
        (t, (!t_fun), d)

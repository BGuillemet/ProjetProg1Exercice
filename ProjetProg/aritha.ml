open X86_64

let args = Sys.argv

exception Wrong_arguments

(* If no argument: read expression from standard input, else from the file in argument *)
let chan, name_output = match Array.length args with
    | 1 -> stdin, "stdin.s"
    | 2 -> if (String.sub args.(1) ((String.length args.(1) - 4)) 4) = ".exp" then
           (open_in args.(1), (String.sub args.(1) 0 ((String.length args.(1) - 4)))^".s")
           else raise Wrong_arguments
    |_ -> raise Wrong_arguments

let lexbuf = Lexing.from_channel chan;;

let read () = Parser.parse Lexer.token lexbuf in
try
    let exp = read () in
    (* Type verification *)
    let exp_type = Typing.find_type_exp exp in
    (* The evaluation of the AST returns 3 bunches of assembly code:
    the main program computing the expression,
    functions called from the main program,
    and data (constants and variables) *)
    let (t, t_fun, d) = Generator.generate_code exp in
    (* Additional text used to print the answer, depending on its type *)
    let printing_text = match exp_type with
    	|Typing.ExpFloat -> movq (reg r15) (reg rdi)
                            ++ pushq (reg rbp)
                            ++ call "print_float"
                            ++ popq rbp
                            ++ ret
                            ++ inline "\n"
                            ++ label "print_float"
                            ++ pushq (reg rbp)
                            ++ leaq (reg ".S_float(%rip)") rdi
                            ++ movl (imm 1) (reg eax)
                            ++ call "printf"
                            ++ movl (imm 0) (reg eax)
                            ++ popq rbp
                            ++ ret ++ inline "\n\n"
	|Typing.ExpInt -> movq (reg r15) (reg rdi)
                          ++ call "print_int"
                          ++ ret
                          ++ inline "\n"
                          ++ label "print_int"
                          ++ movq (reg rdi) (reg rsi)
                          ++ movq (reg "$S_int") (reg rdi)
                          ++ xorq (reg rax) (reg rax)
                          ++ call "printf"
                          ++ ret
	|Typing.NotDef -> raise Typing.Var_error
    in
    (* Putting it all together *)
    let code = { text = globl "main"
                        ++ label "main"
                        ++ t
                        ++ inline "\n"
                        ++ printing_text
                        ++ t_fun
                        ++ inline "\n";
                data = d ++
                       label "S_int" ++ string "%d\n" ++
                       label ".S_float" ++ string "%f\n"; } in
     X86_64.print_in_file name_output code;
with
  Parsing.Parse_error -> prerr_endline "Parse_error"
  | Typing.Type_error -> prerr_endline "Type_error"
  | Lexer.Eof -> exit 0
  | Lexer.Unexpected_character -> print_endline "Unexpected_character"
  | Typing.Var_error -> prerr_endline "Var_error"

(* Definition of an expression as it appears in the AST *)
type exp = Int of int
          |Float of float
          |Var of char
          |Aff of char * exp * exp
          |Pos of exp
          |Neg of exp
          |Plus of exp * exp
          |Minus of exp * exp
          |Times of exp * exp
          |FloatDiv of exp * exp
          |Div of exp * exp
          |Mod of exp * exp
          |FloatPlus of exp * exp
          |FloatMinus of exp * exp
          |FloatTimes of exp * exp
          |Pow of exp * exp
          |Fact of exp
          |OpInt of exp
          |OpFloat of exp

(* Printing the AST as an expression (for debugging, not used) *)
let rec print_op e1 e2 s =
  print_char '(';
  print_exp e1;
  print_string ") ";
  print_string s;
  print_string " (";
  print_exp e2;
  print_char ')'
and print_exp exp = match exp with
  |Int x -> print_int x
  |Float x -> print_float x
  |Var c -> print_char c
  |Aff (c, e, suite) -> print_char c; print_string " = "; print_exp e; print_char '\n'; print_exp suite
  |Pos e -> print_string "pos ("; print_exp e; print_string ")"
  |Neg e -> print_string "neg ("; print_exp e; print_string ")"
  |Plus (e1, e2) -> print_op e1 e2 "+";
  |Minus (e1, e2) -> print_op e1 e2 "-";
  |Times (e1, e2) -> print_op e1 e2 "*";
  |Div (e1, e2) -> print_op e1 e2 "/";
  |Mod (e1, e2) -> print_op e1 e2 "%";
  |FloatPlus (e1, e2) -> print_op e1 e2 "+.";
  |FloatMinus (e1, e2) -> print_op e1 e2 "-.";
  |FloatTimes (e1, e2) -> print_op e1 e2 "*.";
  |FloatDiv (e1, e2) -> print_op e1 e2 "/.";
  |Pow (e1, e2) -> print_op e1 e2 "^"
  |Fact e -> print_char '('; print_exp e; print_string ")!"
  |OpInt e -> print_string "int ("; print_exp e; print_string ")"
  |OpFloat e -> print_string "float ("; print_exp e; print_string ")"

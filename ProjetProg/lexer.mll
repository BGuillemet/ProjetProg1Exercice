{
open Parser
exception Eof
exception Unexpected_character
} 

rule token = parse
  [' ' '\t'] { token lexbuf } (* skip spaces and empty lines *)
| ['\n']     { EOL }
| ['a'-'z'] as c { VAR c } (* Variable names: single letter and lowercase *)
| '='      { EQ }
| ['0'-'9']+ '.' ['0'-'9']* as x    { FLOAT (float_of_string x) } (* float that can have no digit after the . *)
| ['0'-'9']* '.' ['0'-'9']+ as x    { FLOAT (float_of_string x) } (* float that can have no digit before the . *)
| ['0'-'9']+ as x                   { INT (int_of_string x) }
| '('      { LBRACE }
| ')'      { RBRACE }
| "+."     { FLOATPLUS }
| '+'      { PLUS }
| "-."     { FLOATMINUS }
| '-'      { MINUS }
| "*."     { FLOATTIMES }
| '*'      { TIMES }
| "/."     { FLOATDIV }
| '%'      { MOD }
| '/'      { DIV }
| '^'      { POW }
| '!'      { FACT }
| "int"    { OPINT }
| "float"  { OPFLOAT }
| eof { raise Eof } (* signal end of file *)
| _ { raise Unexpected_character }


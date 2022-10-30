/* declaration of tokens from Lexer */
%token <int> INT
%token <float> FLOAT
%token <char> VAR
%token EOL LBRACE RBRACE EOF EQ FLOATPLUS PLUS FLOATMINUS MINUS FLOATTIMES TIMES FLOATDIV MOD DIV POW FACT OPINT OPFLOAT

/* start symbol of the grammar */
%start parse

/* type of syntax tree built by the start symbol */
%type <Asyntax.exp> parse

/* precedence */
%left PLUS, FLOATPLUS, MINUS, FLOATMINUS
%left TIMES, FLOATTIMES, DIV, MOD, FLOATDIV
%left POW, FACT

%%

parse:  
  exp EOL                       { $1 } /* expression */
| VAR EQ exp EOL parse          { Aff ($1, $3, $5) } /* affectation */
;

exp:
  INT                           { Int($1) }
| FLOAT                         { Float($1) }
| VAR                           { Var($1) }
| LBRACE exp RBRACE             { $2 }
| exp FLOATPLUS exp             { FloatPlus($1, $3) }
| exp PLUS exp                  { Plus($1, $3) }
| exp FLOATMINUS exp            { FloatMinus($1, $3) }
| exp MINUS exp                 { Minus($1, $3) }
| exp FLOATTIMES exp            { FloatTimes($1, $3) }
| exp TIMES exp                 { Times($1, $3) }
| exp FLOATDIV exp              { FloatDiv($1, $3) }
| exp DIV exp                   { Div($1, $3) }
| exp MOD exp                   { Mod($1, $3) }
| exp POW exp                   { Pow($1, $3) }
| exp FACT                      { Fact($1) }
| PLUS LBRACE exp RBRACE        { Pos($3) }
| MINUS LBRACE exp RBRACE       { Neg($3) }
| PLUS INT                      { Int($2) }
| MINUS INT                     { Int(-$2) }
| PLUS FLOAT                    { Float($2) }
| MINUS FLOAT                   { Float(-.$2) }
| OPINT LBRACE exp RBRACE       { OpInt($3) }
| OPFLOAT LBRACE exp RBRACE     { OpFloat($3) }
;

%{ (* Emacs, open this file with -*- tuareg -*- *)
   open AST
%}

%token<int> LINT
%token<string> ID
%token IF THEN ELSE RETURN DEF VAR LPAREN RPAREN COMMA
%token EOF SEMICOLON PLUS STAR EQ LBRACE RBRACE

%start<AST.program> program

%nonassoc EQ
%left PLUS
%left STAR

%%

program: ds=definition* EOF
{
  ds
}

definition:
  DEF f=function_identifier
  LPAREN xs=separated_list(COMMA, identifier) RPAREN
  b=block
{
  Fun (f, xs, b)
}
| VAR x=identifier EQ e=expression
{
  Var (x, e)
}

command:
  x=identifier EQ e=expression SEMICOLON
{
  Assign (x, e)
}
| IF e=expression THEN b1=block ELSE b2=block SEMICOLON
{
  If (e, b1, b2)
}
| RETURN e=expression SEMICOLON
{
  Return e
}

block: LBRACE c=command* RBRACE
{
  Block c
}

expression: x=LINT
{
  Int x
}
| x=identifier
{
  Read x
}
| e1=expression b=binop e2=expression
{
  Binop (b, e1, e2)
}
| f=function_identifier LPAREN es=separated_list(COMMA, expression) RPAREN
{
  Call (f, es)
}

%inline binop:
  PLUS { Add   }
| STAR { Mul   }
| EQ   { Equal }

identifier: x=ID
{
  Id x
}

function_identifier: x=ID
{
  FId x
}

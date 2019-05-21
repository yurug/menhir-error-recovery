type program = definition list

and definition =
  | Fun of function_identifier * identifier list * block
  | Var of identifier * expression
  | InvalidDefinition

and block =
  | Block of command list
  | InvalidBlock

and command =
  | Assign of identifier * expression
  | If of expression * block * block
  | Return of expression
  | InvalidCommand

and expression =
  | Int of int
  | Read of identifier
  | Binop of binop * expression * expression
  | Call of function_identifier * expression list
  | InvalidExpression

and binop = Add | Mul | Equal

and function_identifier = FId of string

and identifier = Id of string

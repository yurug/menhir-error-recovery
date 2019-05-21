open Parser
open Parser.MenhirInterpreter

let string_of_token = function
  | VAR -> "VAR"
  | THEN -> "THEN"
  | STAR -> "STAR"
  | SEMICOLON -> "SEMICOLON"
  | RPAREN -> "RPAREN"
  | RETURN -> "RETURN"
  | RBRACE -> "RBRACE"
  | PLUS -> "PLUS"
  | LPAREN -> "LPAREN"
  | LINT d -> Printf.sprintf "LINT(%d)" d
  | LBRACE -> "LBRACE"
  | IF -> "IF"
  | ID s -> Printf.sprintf "ID(%s)" s
  | EQ -> "EQ"
  | EOF -> "EOF"
  | ELSE -> "ELSE"
  | DEF -> "DEF"
  | COMMA -> "COMMA"

let string_of_symbol = function
  | X (T T_error) -> "error"
  | X (T T_VAR) -> "a variable"
  | X (T T_THEN) -> "then"
  | X (T T_STAR) -> "*"
  | X (T T_SEMICOLON) -> ";"
  | X (T T_RPAREN) -> ")"
  | X (T T_RETURN) -> "return"
  | X (T T_RBRACE) -> "}"
  | X (T T_PLUS) -> "+"
  | X (T T_LPAREN) -> "("
  | X (T T_LINT) -> "an integer"
  | X (T T_LBRACE) -> "{"
  | X (T T_IF) -> "if"
  | X (T T_ID) -> "an identifier"
  | X (T T_EQ) -> "="
  | X (T T_EOF) -> "end-of-file"
  | X (T T_ELSE) -> "else"
  | X (T T_DEF) -> "def"
  | X (T T_COMMA) -> ","
  | X (N N_loption_separated_nonempty_list_COMMA_identifier__) ->
       "a list of identifiers separated by commas"
  | X (N N_separated_nonempty_list_COMMA_identifier_) ->
       "a list of identifiers separated by commas"
  | X (N N_loption_separated_nonempty_list_COMMA_expression__) ->
       "a list of expressions separated by commas"
  | X (N N_separated_nonempty_list_COMMA_expression_) ->
       "a list of expressions separated by commas"
  | X (N N_program) ->
       "a program"
  | X (N N_list_definition_) ->
       "a list of definitions"
  | X (N N_list_command_) ->
       "a list of commands"
  | X (N N_identifier) ->
       "an identifier"
  | X (N N_function_identifier) ->
       "a function identifier"
  | X (N N_expression) ->
       "an expression"
  | X (N N_definition) ->
       "a definition"
  | X (N N_command) ->
       "a command"
  | X (N N_block) ->
       "a block of commands"

let string_of_item (p, i) =
  string_of_symbol (lhs p) ^ " -> "
  ^ String.concat " " (
        List.mapi (fun j s ->
            (if j = i then "." else "") ^ string_of_symbol s)
          (rhs p))
  ^ (if i = List.length (rhs p) then "." else "")

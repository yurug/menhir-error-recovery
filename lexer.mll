{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open Parser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let identifier = ['a'-'z' '_' 'A'-'Z']+

rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  | "(*"            { comment 1 lexbuf           }

  | "var"           { VAR  }
  | "def"           { DEF  }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "return"        { RETURN }

  (** Identifiers *)
  | identifier as i  { ID i  }

  (** Literals *)
  | (['-']? digit+) as d     { LINT (int_of_string d) }

  (** Infix operators *)
  | "="             { EQ          }
  | "+"             { PLUS        }
  | "*"             { STAR        }

  (** Punctuation *)
  | ";"             { SEMICOLON }
  | "("             { LPAREN    }
  | ")"             { RPAREN    }
  | "{"             { LBRACE    }
  | "}"             { RBRACE    }
  | ","             { COMMA     }
  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." (fun _ -> EOF) }

and comment level = parse
  | "*)" {
    if level = 1 then
      token lexbuf
    else
      comment (pred level) lexbuf
  }
  | "(*" {
      comment (succ level) lexbuf
  }
  | eof {
    error lexbuf "unterminated comment." (fun _ -> EOF)
  }
  | newline {
    next_line_and (comment level) lexbuf
  }
  | _ {
    comment level lexbuf
  }

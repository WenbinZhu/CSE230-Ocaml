{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

let letter = ['A'-'Z''a'-'z']
let digit = ['0'-'9']

rule token = parse
    eof          { EOF }
  | "let"        { LET }
  | "="          { EQ }
  (* ADD LEXING RULES FOR OTHER TOKENS HERE *)
  | "true"       { TRUE }
  | "false"      { FALSE }
  | "rec"        { REC }
  | "in"         { IN }
  | "fun"        { FUN }
  | "->"         { ARROW }
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }

  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { MUL }
  | "/"          { DIV }
  | "<"          { LT }
  | "<="         { LE }
  | "!="         { NE }
  | "&&"         { AND }
  | "||"         { OR }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "["          { LBRAC }
  | "]"          { RBRAC }
  | ";"          { SEMI } 
  | "::"         { COLONCOLON }

  | digit+ as i                   { Num (int_of_string i) }
  (* Note: The order of these rules matter, so keep Id after all other lexemes *)
  | letter (letter|digit)* as s   { Id s }
  | [' ' '\n' '\r' '\t']          { token lexbuf }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }

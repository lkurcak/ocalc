open Ocalc
open Ocalc.Expr

let () =
  let expr = Div (Sub (Add (Num 2, Mul (Num 3, Num 4)), Num 1), Num 1) in
  let result = eval expr in
  Printf.printf "Result: %d\n" result

let () =
  let tokens = Lexer.tokenize "2 + 3 * 4" in
  List.iter
    (fun t ->
      match t with
      | Lexer.INT n -> Printf.printf "INT(%d) " n
      | Lexer.PLUS -> Printf.printf "PLUS "
      | Lexer.MINUS -> Printf.printf "MINUS "
      | Lexer.TIMES -> Printf.printf "TIMES "
      | Lexer.DIV -> Printf.printf "DIV "
      | Lexer.LPAREN -> Printf.printf "LPAREN "
      | Lexer.RPAREN -> Printf.printf "RPAREN ")
    tokens;
  Printf.printf "\n"

let () =
  let expr = Parser.parse_expr "2 + 3 * 4" in
  Printf.printf "Result: %d\n" (eval expr)

open Lexer

type parse_state = { tokens : token list; mutable pos : int }

let current state =
  if state.pos < List.length state.tokens then
    Some (List.nth state.tokens state.pos)
  else None

let advance state = state.pos <- state.pos + 1

let parse_expr str =
  let tokens = Lexer.tokenize str in
  let state = { tokens; pos = 0 } in

  let rec parse_add () =
    let rec loop left =
      match current state with
      | Some PLUS ->
          advance state;
          let right = parse_mul () in
          loop (Expr.Add (left, right))
      | Some MINUS ->
          advance state;
          let right = parse_mul () in
          loop (Expr.Sub (left, right))
      | _ -> left
    in
    loop (parse_mul ())
  and parse_mul () =
    let rec loop left =
      match current state with
      | Some TIMES ->
          advance state;
          let right = parse_primary () in
          loop (Expr.Mul (left, right))
      | Some DIV ->
          advance state;
          let right = parse_primary () in
          loop (Expr.Div (left, right))
      | _ -> left
    in
    loop (parse_primary ())
  and parse_primary () =
    match current state with
    | Some (INT n) ->
        advance state;
        Expr.Num n
    | Some LPAREN ->
        advance state;
        let e = parse_add () in
        (match current state with
        | Some RPAREN -> advance state
        | _ -> failwith "Expected )");
        e
    | _ -> failwith "Expected number or ("
  in
  parse_add ()

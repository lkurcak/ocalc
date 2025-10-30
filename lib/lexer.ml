type token = INT of int | PLUS | MINUS | TIMES | DIV | LPAREN | RPAREN

let tokenize str =
  let len = String.length str in
  let rec loop i acc =
    if i >= len then List.rev acc
    else
      let c = str.[i] in
      match c with
      | ' ' -> loop (i + 1) acc (* skip whitespace *)
      | '+' -> loop (i + 1) (PLUS :: acc)
      | '-' -> loop (i + 1) (MINUS :: acc)
      | '*' -> loop (i + 1) (TIMES :: acc)
      | '/' -> loop (i + 1) (DIV :: acc)
      | '(' -> loop (i + 1) (LPAREN :: acc)
      | ')' -> loop (i + 1) (RPAREN :: acc)
      | '0' .. '9' ->
          (* read full number *)
          let j = ref (i + 1) in
          while !j < len && str.[!j] >= '0' && str.[!j] <= '9' do
            incr j
          done;
          let num_str = String.sub str i (!j - i) in
          let num = int_of_string num_str in
          loop !j (INT num :: acc)
      | _ -> failwith (Printf.sprintf "Unknown char: %c" c)
  in
  loop 0 []

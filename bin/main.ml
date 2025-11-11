open Ocalc
open Ocalc.Expr

let calculate_and_print input =
  try
    let expr = Parser.parse_expr input in
    let result = eval expr in
    Printf.printf "%d\n" result
  with
  | Failure msg -> Printf.eprintf "Error: %s\n" msg; exit 1
  | Division_by_zero -> Printf.eprintf "Error: Division by zero\n"; exit 1

let () =
  let args = Array.to_list Sys.argv in
  match List.tl args with
  | [] ->
      (* No arguments: read from stdin interactively *)
      (try
         print_string "Enter expression: ";
         flush stdout;
         let input = read_line () in
         calculate_and_print input
       with End_of_file -> ())
  | expressions ->
      (* Arguments provided: evaluate each one *)
      List.iter calculate_and_print expressions

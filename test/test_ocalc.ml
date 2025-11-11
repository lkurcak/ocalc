open Ocalc
open Ocalc.Expr

let test_parse_and_eval input expected =
  let expr = Parser.parse_expr input in
  let result = eval expr in
  if result = expected then
    Printf.printf "✓ %s = %d\n" input expected
  else
    Printf.printf "✗ %s: expected %d, got %d\n" input expected result

let () =
  (* Basic operations *)
  test_parse_and_eval "2 + 3" 5;
  test_parse_and_eval "2 * 3" 6;
  test_parse_and_eval "2 + 3 * 4" 14;
  test_parse_and_eval "(2 + 3) * 4" 20;
  test_parse_and_eval "10 - 5" 5;
  test_parse_and_eval "10 / 5" 2;
  test_parse_and_eval "10 / 4" 2;
  test_parse_and_eval "10 / 3" 3;
  test_parse_and_eval "10 / 2" 5;
  test_parse_and_eval "10 / 1" 10;
  
  (* Left associativity tests *)
  test_parse_and_eval "4 / 2 / 2" 1;
  test_parse_and_eval "2 - 1 - 1" 0;
  test_parse_and_eval "100 - 20 - 30 - 10" 40;
  test_parse_and_eval "16 / 4 / 2" 2;
  test_parse_and_eval "8 / 4 / 2 / 1" 1;
  
  (* Mixed operations with precedence *)
  test_parse_and_eval "2 + 3 * 4 - 5" 9;
  test_parse_and_eval "10 - 2 * 3" 4;
  test_parse_and_eval "20 / 4 + 3 * 2" 11;
  test_parse_and_eval "100 / 10 / 2 + 1" 6;
  test_parse_and_eval "2 * 3 + 4 * 5" 26;
  test_parse_and_eval "50 - 10 * 2 - 5" 25;
  
  (* Complex parentheses *)
  test_parse_and_eval "(10 - 6) / 2" 2;
  test_parse_and_eval "((8 + 2) * 3) - 5" 25;
  test_parse_and_eval "(100 - 50) / (10 - 5)" 10;
  test_parse_and_eval "((2 + 3) * (4 + 1)) - 10" 15;
  test_parse_and_eval "3 * (4 + 5) - 2 * (6 - 1)" 17;
  
  (* Chain operations *)
  test_parse_and_eval "1 + 2 + 3 + 4 + 5" 15;
  test_parse_and_eval "2 * 3 * 4" 24;
  test_parse_and_eval "20 - 5 - 3 - 2" 10;
  test_parse_and_eval "1000 / 10 / 10" 10;
  
  (* Tricky precedence combinations *)
  test_parse_and_eval "5 - 2 * 2 + 3" 4;
  test_parse_and_eval "10 / 2 - 3" 2;
  test_parse_and_eval "2 + 10 / 5 * 3" 8;
  test_parse_and_eval "16 / 8 * 4 / 2" 4;
  test_parse_and_eval "3 + 4 * 2 / (1 + 1)" 7;
  
  Printf.printf "Tests done\n"

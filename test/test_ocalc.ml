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
  Printf.printf "Tests done\n"

(* Exercise 4. checkmetro *)
open Ex4
open Testlib

let rec string_of_metro m =
  match m with
  | STATION name -> Printf.sprintf "Station \"%s\"" name
  | AREA (name, m') -> Printf.sprintf "Area \"%s\" (%s)" name (string_of_metro m')
  | CONNECT (m1, m2) -> Printf.sprintf "Connect (%s), (%s)" (string_of_metro m1) (string_of_metro m2)

module TestEx4: TestEx =
  struct
    let exnum = 4

    type testcase =
      | CHECK of metro * bool

    let runner tc =
      match tc with
      | CHECK (m, ans) -> checkMetro m = ans

    let string_of_tc tc =
      match tc with
      | CHECK (m, ans) -> (Printf.sprintf "checkMetro (%s)" (string_of_metro m), string_of_bool (checkMetro m), string_of_bool ans)

    let testcases =
      [ CHECK (AREA ("a", STATION "a"), true)
      ; CHECK (AREA ("a", AREA ("a", STATION "a")), true)
      ; CHECK (AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "b"))), true)
      ; CHECK (AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "a"))), true)
      ; CHECK (AREA ("a", STATION "b"), false)
      ; CHECK (AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "c"))), false)
      ; CHECK (AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "c"))), false)
      ; CHECK (AREA ("one", CONNECT (CONNECT (STATION "one", CONNECT (STATION "one", STATION "one")), STATION "one")), true)
      ; CHECK (AREA ("a", AREA ("b", STATION "a")), true)
      ; CHECK (STATION "a", false)
      ; CHECK (STATION "b", false)
      ; CHECK (AREA ("b", STATION "b"), true)
      ; CHECK (AREA ("asdf", STATION "asdf"), true)
      ; CHECK (AREA ("b", STATION "a"), false)
      ; CHECK (AREA ("seoul", STATION "busan"), false)
      ; CHECK (CONNECT (AREA ("seoul", STATION "seoul"), AREA ("busan", STATION "busan")), true)
      ; CHECK (CONNECT (AREA ("seoul", STATION "seoul"), AREA ("busan", STATION "seoul")), false)
      ; CHECK (CONNECT (AREA ("seoul", STATION "busan"), AREA ("busan", STATION "busan")), false)
      ; CHECK (CONNECT (AREA ("seoul", STATION "busan"), AREA ("busan", STATION "seoul")), false)
      ; CHECK (CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA ("a", CONNECT (STATION "b", STATION "a")))), true)
      ; CHECK (CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA ("a", CONNECT (STATION "b", STATION "c")))), false)
      ]
  end

open TestEx4
let _ = wrapper exnum testcases runner string_of_tc

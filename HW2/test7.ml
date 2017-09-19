(* Exercise 7. Zexpr *)
open Ex7
open Testlib
open Printf

open Zexpr

let rec string_of_expr e =
  match e with
  | NUM n -> string_of_int n
  | PLUS (e1, e2) -> sprintf "(%s) + (%s)" (string_of_expr e1) (string_of_expr e2)
  | MINUS (e1, e2) -> sprintf "(%s) - (%s)" (string_of_expr e1) (string_of_expr e2)
  | MULT (e1, e2) -> sprintf "(%s) / (%s)" (string_of_expr e1) (string_of_expr e2)
  | DIVIDE (e1, e2) -> sprintf "(%s) * (%s)" (string_of_expr e1) (string_of_expr e2)
  | MAX es -> sprintf "max(%s)" (String.concat ", " (List.map string_of_expr es))
  | VAR name -> name
  | LET (name, e1, e2) -> sprintf "LET %s = %s in (%s)" name (string_of_expr e1) (string_of_expr e2)

module TestEx7: TestEx =
  struct
    let exnum = 7

    type testcase =
      | EVAL of expr * int

    let runner tc =
      match tc with
      | EVAL (e, ans) -> eval(emptyEnv, e) = eval(emptyEnv, NUM ans)
          (*
          let _ = print_endline "- two line below should be same" in
          let _ = print_string "output: "; print_value (eval(emptyEnv, e)); print_endline "" in
          let _ = print_string "answer: "; print_int v; print_endline "" in true
*)
    let string_of_tc tc =
      match tc with
      | EVAL (e, ans) ->
          ( sprintf "eval(emptyEnv, %s)" (string_of_expr e)
          , string_of_expr (NUM ans)
          , "" (* string_of_expr (NUM (eval(emptyEnv, e))) *)
          )

    let testcases =
      [ EVAL (NUM 1, 1)
      ; EVAL (NUM (-10), -10)
      ; EVAL (PLUS (NUM 123, NUM 456), 579)
      ; EVAL (MINUS (NUM 456, NUM 123), 333)
      ; EVAL (MULT (NUM 25, NUM 16), 400)
      ; EVAL (DIVIDE (NUM (-3000), NUM 22), -136)
      ; EVAL (MAX [], 0)
      ; EVAL (MAX [NUM (-98765)], -98765)
      ; EVAL (MAX [NUM 10; NUM 20; NUM 30], 30)
      ; EVAL (LET ("x", NUM 15, VAR "x"), 15)
      ; EVAL (LET ("x", NUM 17, LET ("y", VAR "x", LET ("x", NUM 30, VAR "y"))), 17)
      ]
  end

open TestEx7
let _ = wrapper exnum testcases runner string_of_tc

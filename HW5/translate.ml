(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  let push_val = fun x -> Sm5.PUSH (Sm5.Val x)
  let push_id = fun x -> Sm5.PUSH (Sm5.Id x)


  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [push_val (Sm5.Z i)]
    | K.TRUE -> [push_val (Sm5.B true)]
    | K.FALSE -> [push_val (Sm5.B false)]
    | K.UNIT -> [push_val (Sm5.Unit)]
    | K.VAR x -> [push_id x; Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e -> trans e @ [Sm5.NOT]
    | K.ASSIGN (x, e) -> trans e @ [push_id x; Sm5.STORE; push_id x; Sm5.LOAD]
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.IF (e, e1, e2) -> trans e @ [Sm5.JTR (trans e1, trans e2)]
    | K.WHILE (e1, e2) -> begin
      let w = "!while" in
      let cond = "!cond" in
      let body = K.SEQ (e2, K.CALLV (w, e1)) in
      let f = K.IF (K.VAR cond, body, K.UNIT) in
      trans (K.LETF (w, cond, f, K.CALLV(w, e1)))
    end
    | K.FOR (x, e1, e2, e3) -> begin
      let f_idx = "!idx" in
      let f_num = "!num" in
      let f_cond = K.NOT (K.LESS (K.VAR f_num, K.VAR f_idx)) in
      let body = K.SEQ (K.ASSIGN (x, K.VAR f_idx),
        K.SEQ (e3, K.ASSIGN (f_idx, K.ADD (K.VAR f_idx, K.NUM 1)))) in
      let f_to_while = K.WHILE (f_cond, body) in
      trans (K.LETV (f_idx, e1, K.LETV (f_num, e1, f_to_while)))
    end
    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; push_id x; Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) ->
        [Sm5.PUSH (Sm5.Fn (x, Sm5.BIND f :: trans e1)); Sm5.BIND f] @
        trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.CALLV (x, e) ->
        [push_id x; push_id x] @ trans e @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, x) ->
        [push_id f; push_id f; push_id x; Sm5.LOAD; push_id x; Sm5.CALL]
    | K.READ x -> [Sm5.GET; push_id x; Sm5.STORE; push_id x; Sm5.LOAD]
    | K.WRITE e ->
      let tmp = "!tmp" in
      trans e @ [Sm5.MALLOC; Sm5.BIND tmp; push_id tmp; Sm5.STORE; push_id tmp]
      @ [Sm5.LOAD; Sm5.PUT; push_id tmp; Sm5.LOAD; Sm5.UNBIND]

end

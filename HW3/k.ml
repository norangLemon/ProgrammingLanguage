(*
 * SNU 4190.310 Programming Languages 2017 Fall
 *  K- Interpreter Skeleton Code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM =
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c ->
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)

  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  module Rec = Map.Make(String)

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr"))
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | READ x ->
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)
    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')
    | NUM n -> (Num (n), mem)
    | TRUE -> (Bool (true), mem)
    | FALSE -> (Bool (false), mem)
    | UNIT -> (Unit, mem)
    | VAR x ->
      let adr = lookup_env_loc env x in
      let ret = Mem.load mem adr in
      (ret, mem)
    | ADD (exp1, exp2) ->
      let (n1, mem') = eval mem env exp1 in
      let (n2, mem'') = eval mem' env exp2 in
      let ret =
        match (n1, n2) with
        | (Num a, Num b) -> a + b
        | _ -> raise (Error "TypeError : not int")
      in
      (Num (ret), mem'')
    | SUB (exp1, exp2) ->
      let (n1, mem') = eval mem env exp1 in
      let (n2, mem'') = eval mem' env exp2 in
      let ret =
        match (n1, n2) with
        | (Num a, Num b) -> a - b
        | _ -> raise (Error "TypeError : not int")
      in
      (Num (ret), mem'')
    | MUL (exp1, exp2) ->
      let (n1, mem') = eval mem env exp1 in
      let (n2, mem'') = eval mem' env exp2 in
      let ret =
        match (n1, n2) with
        | (Num a, Num b) -> a * b
        | _ -> raise (Error "TypeError : not int")
      in
      (Num (ret), mem'')
    | DIV (exp1, exp2) ->
      let (n1, mem') = eval mem env exp1 in
      let (n2, mem'') = eval mem' env exp2 in
      let ret =
        match (n1, n2) with
        | (Num a, Num b) -> a / b
        | _ -> raise (Error "TypeError : not int")
      in
      (Num (ret), mem'')
    | EQUAL (exp1, exp2) ->
      let (v1, mem') = eval mem env exp1 in
      let (v2, mem'') = eval mem' env exp2 in
      let ret =
        match (v1, v2) with
        | (Num a, Num b) -> (a = b)
        | (Bool a, Bool b) -> (a = b)
        | (Unit, Unit) -> true
        | _ -> false
      in
      (Bool (ret), mem'')
    | LESS (exp1, exp2) ->
      let (n1, mem') = eval mem env exp1 in
      let (n2, mem'') = eval mem' env exp2 in
      let ret =
        match (n1, n2) with
        | (Num a, Num b) -> (a < b)
        | _ -> raise (Error "TypeError : not int")
      in
      (Bool (ret), mem'')
    | NOT e ->
      let (x, mem') = eval mem env e in
      let ret =
        match x with
        | Bool b -> not b
        | _ -> raise (Error "TypeError : boolean is expected")
      in
      (Bool (ret), mem')
    | SEQ (exp1, exp2) ->
      let (v1, mem') = eval mem env exp1 in
      let (v2, mem'') = eval mem' env exp2 in
      (v2, mem'')
    | IF (e_, exp1, exp2) ->
      let (x, mem') = eval mem env e_ in
      let cond = value_bool x in
      let (v, mem'') =
        if (cond = true) then (eval mem' env exp1)
        else (eval mem' env exp2)
      in
      (v, mem'')
    | WHILE (exp1, exp2) ->
      let (x, mem') = eval mem env exp1 in
      let (v1, mem1) = eval mem' env exp2 in
      if value_bool x 
        then eval mem1 env e
        else (Unit, mem')
    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in
      let (l, mem'') = Mem.alloc mem' in
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (x, l, e1, e2) ->
      let env' = (Env.bind env x (Proc (l, e1, env))) in
      let (v, mem') = eval mem env' e2 in
      (v, mem')
    | CALLV (f, l) ->
      let rec get_val_list = fun mem_ env_ exp_list v_list ->
        match exp_list with
          | hd :: tl ->
          begin
            let (v_, m_) = eval mem_ env_ hd in
            (get_val_list m_ env_ tl (v_list @ [v_]))
          end
        | [] -> (v_list, mem_)
      in
      let rec addval = fun mem_ env_ id_list_ v_list_ ->
        match (id_list_, v_list_) with
        | ((x_::id_tail), (v_::v_tail)) ->
        begin
          let (l, mem'_) = Mem.alloc mem_ in
          addval (Mem.store mem'_ l v_) (Env.bind env_ x_ (Addr l)) id_tail v_tail
        end
        | ([], []) -> (env_, mem_)
        | _ -> raise (Error "InvalidArg")
      in
      let (id_list, exp', env') = lookup_env_proc env f in
      let (v_list, mem') = get_val_list mem env l [] in
      let (env'', mem'') = addval mem' env' id_list v_list in
      (eval mem'' (Env.bind env'' f (Proc (id_list, exp', env'))) exp')
    | CALLR (f, arg_list) ->
    begin
      let (id_list, exp_f, env_f) = lookup_env_proc env f in
      let rec add_ref =
      begin
        fun env_ arg_list_ id_list_ ->
        match (arg_list_, id_list_) with
        | (arg::arg_tl, id::id_tl) ->
          let l_ = lookup_env_loc env arg in
          (add_ref (Env.bind env_ id (Addr l_)) arg_tl id_tl)
        | ([], []) -> env_
        | _ -> raise (Error "InvalidArg")
      end in
      let env_ref_add = add_ref env_f arg_list id_list in
      (eval mem (Env.bind env_ref_add f (Proc (id_list, exp_f, env_f))) exp_f)
    end
    | RECORD id_exp_list ->
    begin
      match id_exp_list with
      | [] -> (Unit, mem)
      | _ ->
      begin
        let rec calc = fun l_ mem_ id_v_list ->
          match l_ with
          | (id, exp_) :: tail ->
            let (v, mem') = eval mem_ env exp_ in
            (calc tail mem' (id_v_list @ [(id, v)]))
          | [] -> (mem_, id_v_list)
        in
        let rec assign = fun record_ id_v_list mem_ ->
          match id_v_list with
          | (id, v) :: tl ->
            let (l, mem') = Mem.alloc mem_ in
            (assign (Rec.add id l record_) tl (Mem.store mem' l v))
          | [] -> (record_, mem_)
        in
        let (mem', id_v_list) = calc id_exp_list mem [] in
        let (record, mem'') = assign Rec.empty id_v_list mem' in
        let ret x =
          try Rec.find x record
          with Not_found -> raise (Error "Unbound")
        in
        (Record (ret), mem'')
      end
    end
    | FIELD (exp, id) ->
      let (v, mem') = eval mem env exp in
      let r = value_record v in
      (Mem.load mem' (r id), mem')
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
    | ASSIGNF (exp1, id, exp2) ->
      let (v_r, mem1) = eval mem env exp1 in
      let (v, mem2) = eval mem1 env exp2 in
      let r = value_record v_r in
      let l = r id in
      (v, Mem.store mem2 l v)

  let run (mem, env, pgm) =
    let (v, _ ) = eval mem env pgm in
    v
end

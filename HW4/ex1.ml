type require = id * (cond list)
and cond
    = Items of gift list (* 선물들 *)
    | Same of id (* 어느 조카의 선물들 *)
    | Common of cond * cond (* 두조건에 공통된 선물들 *)
    | Except of cond * gift list (* 조건에서 어느 선물들은 빼고 *)
and gift = int (* 선물 번호 *)
and id = A | B | C | D | E (* 조카 이름 *)
and assign = id * (gift list)

let rec erase: gift list -> gift -> gift list =
    fun l x ->
        match l with
        | hd :: tl -> (
            if (hd = x) then tl
            else hd :: (erase tl x)
        )
        | _ -> []

let rec subtract_list: gift list -> gift list -> gift list =
    fun l0 l1 ->
        match l1 with
        | hd :: tl -> (
            (subtract_list (erase l0 hd) tl)
        )
        | _ -> l0

let rec have: gift list -> gift -> bool =
    fun l g ->
        match l with
        | hd :: tl -> (
            if (hd = g) then true
            else (have tl g)
        )
        | _ -> false

let rec intersection: gift list -> gift list -> gift list =
    fun l0 l1 ->
        match l1 with
        | hd :: tl -> (
            let next = intersection l0 tl in
            if (have l0 hd) then hd :: next
            else next
        )
        | _ -> []

let sort_u = fun a -> List.sort_uniq compare a

let merge_gift = fun a b -> (sort_u (List.merge compare a b))

let get_gift_list: assign list -> id -> gift list = fun a_list i ->
    snd (List.find (fun x -> if (fst x = i) then true else false) a_list)

let rec gift_after_cond: assign list -> cond -> gift list =
    fun a_list con -> (
        match con with
        | Items g_list -> sort_u g_list
        | Same s -> (get_gift_list a_list s)
        | Common (c1, c2) -> (
            let l1 = gift_after_cond a_list c1 in
            let l2 = gift_after_cond a_list c2 in
            intersection l1 l2
        )
        | Except (c, g_list) -> (
            let l = gift_after_cond a_list c in
            subtract_list l g_list
        )
    )

let next_gift: assign list -> cond list -> gift list =
    fun a_list c_list -> (
        let concat_next = fun g c -> g @ (gift_after_cond a_list c) in
        let temp_list = List.fold_left concat_next [] c_list in
        sort_u temp_list
    )


let next_assign: assign list -> require list -> assign list =
    fun a_list r_list -> (
        let new_gl = fun x -> (next_gift a_list (snd x)) in
        let prev_gl = fun r -> (get_gift_list a_list (fst r)) in
        let new_a = fun r -> ((fst r), merge_gift (prev_gl r) (new_gl r)) in
        (List.fold_left (fun a r -> a @ [(new_a r)]) [] r_list)
    )

let compare_ : 'a -> 'a -> int =
    fun a b -> (
        let a_ = fst a in
        let b_ = fst b in
        if (a_ > b_) then 1
        else -1
    )

let rec get_final: assign list -> require list -> assign list =
    fun a_list r_list -> (
        let next = next_assign a_list r_list in
        if (next = a_list) then a_list
        else (get_final next r_list)
    )

let shoppingList: require list -> assign list =
    let sort_r = fun x -> List.sort compare_ x in
    let empty_assign = [(A, []); (B, []); (C, []); (D, []); (E, [])] in
    fun x -> get_final empty_assign (sort_r x)

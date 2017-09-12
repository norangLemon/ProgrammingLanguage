type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat = fun (a, b) ->
    match a with
    | ZERO -> b
    | SUCC n -> natadd (a, SUCC b)

let rec netmul: nat * nat -> nat = fun (a, b) ->
    match b with
    | ZERO -> ZERO
    | SUCC n -> natadd (netmul(a, n), a)

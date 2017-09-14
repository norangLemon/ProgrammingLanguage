let rec iter x = fun (n, f) ->
    if (n =< 0) 
        then x
        else iter (n-1, f) (f x)

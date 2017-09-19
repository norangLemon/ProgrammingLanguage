type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (a, b) ->
    match (a, b) with
    | (NIL, NIL) -> 0
    | (ONE(c), ONE(d)) -> 2 + 2 * crazy2add(c, d)
    | (MONE(c), MONE(d)) -> -2 + 2 * crazy2add(c, d)
    | (ZERO(c), ONE(d)) | (ONE(c), ZERO(d)) -> 1 + 2 * crazy2add(c, d)
    | (ZERO(c), MONE(d)) | (MONE(c), ZERO(d)) -> -1 + 2 * crazy2add(c, d)
    | (ZERO(c), ZERO(d)) | (ONE(c), MONE(d)) | (MONE(c), ONE(d)) | (NIL, ZERO(c)) | (ZERO(c), NIL)  -> 2*(crazy2add(c, d))
    | (NIL, ONE(c)) | (ONE, NIL(c)) -> 1 + crazy2add(c, NIL)
    | (NIL, MONE(c)) | (MONE, NIL(c)) -> -1 + crazy2add(c, NIL)

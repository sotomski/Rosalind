// http://rosalind.info/problems/fib/

// k = 3
(*
    month   newBunnies      oldBunnies      total
    1       1               0               1
    2       0               1               1
    3       k               1               k+1
    4       k               k+1             2k+1
    5       k(k+1)          2k+1            k^2+3k+2
    6       k(2k+1)         k^2+3k+2        
    n       k*F(n-2)        F(n-1)          k*F(n-2) + F(n-1)
*)

open System

let rec f (n:long) (k:long) : long =
    match n with
    | 1 -> 1
    | 2 -> 1
    | _ -> (f (n-1) k) + (k * (f (n-2) k))

f 5 3

f 40 5

f 29 5
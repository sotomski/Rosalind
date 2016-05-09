// http://rosalind.info/problems/subs/

open System

let main (input:string) =
    let splittedInput = input.Split ([| |])
    let genome = splittedInput.[0]
    let motif = splittedInput.[1].ToCharArray ()

    genome
    |> Seq.windowed motif.Length
    |> Seq.indexed
    |> Seq.filter (fun (i,x) -> 
        match (fun x y -> if x = y then 0 else 1) x motif with
        | 0 -> true
        | _ -> false)
    |> Seq.iter (fun (i, x) -> printf "%d " (i+1))

    0

let input = @"GATATATGCATATACTT
ATAT"

main input // 2 4 10
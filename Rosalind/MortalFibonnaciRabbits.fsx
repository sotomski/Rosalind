// http://rosalind.info/problems/fibd/

// Note: Below algorithm is super naive. I just wanted to try doing it iteratively.

let f2 n m =
    let ageByOneMonth bunnies = 
        bunnies 
        |> List.permute (fun idx -> (idx + 1) % bunnies.Length)
        |> List.mapi (fun i x -> if i = 0 then 0UL else x)
    
    let breed bunnies =
        let newBunnies = bunnies
                            |> List.mapi (fun i x -> if i < 2 then 0UL else x)
                            |> List.sum
        bunnies
        |> List.indexed
        |> List.filter (fun (i, x) -> i <> 0)
        |> List.map (fun (_, x) -> x)
        |> List.append [newBunnies]

    let liveOneMonth bunnies =
        bunnies
        |> ageByOneMonth
        |> breed

    let bunnies = [for c in [1..m+1] -> if c = 1 then 1UL else 0UL]

    [2 .. n]
    |> List.fold (fun bunnies x -> liveOneMonth bunnies) bunnies
    |> List.take (bunnies.Length - 1) // Don't count the dead
    |> List.sum

f2 88 18 //4
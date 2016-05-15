// http://rosalind.info/problems/perm/

// WARNING: May cause session termination in FSI. Works perfectly as a standalone application.

let src = 4

let generatePermutations input =
    let addNewLayerOfPermutation l =
        input
        |> List.choose (fun srcX ->
            match List.contains srcX l with
            | false -> Some (List.append l [srcX])
            | true -> None )

    let rec recGeneratePermutations howManyStillToGenerate source =
        match howManyStillToGenerate with
        | 1 -> List.map (fun x -> [x]) input
        | x -> 
            recGeneratePermutations (howManyStillToGenerate - 1) source
            |> List.collect (fun prevList -> addNewLayerOfPermutation prevList)

    recGeneratePermutations (List.length input) input


let res = [ 1 .. src ] |> generatePermutations

printfn "%d" (List.length res)
res
|> List.iter (fun l -> 
    l |> List.iter (fun x -> printf "%d " x)
    printfn "")
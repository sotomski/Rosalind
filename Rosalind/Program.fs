// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 

    let src = 6
    
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

    0 // return an integer exit code


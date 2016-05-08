// http://rosalind.info/problems/gc/

open System

type fastaRecord = { Id:string ; Strand:string }

let parseFASTA (input:string) =
    input.Split ([| |])
    |> Array.fold (fun result (el:string) ->
                                if el.StartsWith ">Rosalind" then
                                    let newRec = { Id = el; Strand = "" }
                                    newRec::result
                                else
                                    let current = result.Head
                                    let updated = { current with Strand = current.Strand + el }
                                    result
                                    |> List.except [current]
                                    |> List.append [updated]) []
    |> List.filter (fun r -> r.Id.StartsWith ">Rosalind")

let computeGc fastaRec =
    let gcs = fastaRec.Strand |> Seq.filter (fun c -> c = 'C' || c = 'G') |> Seq.length
    (fastaRec, float gcs / float fastaRec.Strand.Length * 100.0)

let main input =
    input
    |> parseFASTA
    |> List.map (fun r -> computeGc r)
    |> List.maxBy (fun (fasta, gc) -> gc)

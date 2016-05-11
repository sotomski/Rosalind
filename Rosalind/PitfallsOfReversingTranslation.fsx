// http://rosalind.info/problems/mrna/

let codonTable = [| ("UUU", "F"); ("CUU", "L"); ("AUU", "I"); ("GUU", "V");
                    ("UUC", "F"); ("CUC", "L"); ("AUC", "I"); ("GUC", "V");
                    ("UUA", "L"); ("CUA", "L"); ("AUA", "I"); ("GUA", "V");
                    ("UUG", "L"); ("CUG", "L"); ("AUG", "M"); ("GUG", "V");
                    ("UCU", "S"); ("CCU", "P"); ("ACU", "T"); ("GCU", "A");
                    ("UCC", "S"); ("CCC", "P"); ("ACC", "T"); ("GCC", "A");
                    ("UCA", "S"); ("CCA", "P"); ("ACA", "T"); ("GCA", "A");
                    ("UCG", "S"); ("CCG", "P"); ("ACG", "T"); ("GCG", "A");
                    ("UAU", "Y"); ("CAU", "H"); ("AAU", "N"); ("GAU", "D");
                    ("UAC", "Y"); ("CAC", "H"); ("AAC", "N"); ("GAC", "D");
                    ("UAA", "_"); ("CAA", "Q"); ("AAA", "K"); ("GAA", "E");
                    ("UAG", "_"); ("CAG", "Q"); ("AAG", "K"); ("GAG", "E");
                    ("UGU", "C"); ("CGU", "R"); ("AGU", "S"); ("GGU", "G");
                    ("UGC", "C"); ("CGC", "R"); ("AGC", "S"); ("GGC", "G");
                    ("UGA", "_"); ("CGA", "R"); ("AGA", "R"); ("GGA", "G");
                    ("UGG", "W"); ("CGG", "R"); ("AGG", "R"); ("GGG", "G"); |]

let aminoAcidCodonCount = 
    codonTable 
    |> Seq.countBy (fun (codon, aminoAcid) -> aminoAcid)
    |> Seq.map (fun (a, c) -> (Seq.head a ,c))

let stopCount = 
    let (_, count) = Seq.find (fun (a, c) -> a = '_') aminoAcidCodonCount
    count

let inferMrna protein = 0

let main (input:string) =
    let modFactor = 1000000

    input
    |> Seq.map (fun acid -> 
        let (aa, cc) = Seq.find (fun (a, count) -> a = acid) aminoAcidCodonCount 
        cc)
    |> Seq.append [stopCount]
    |> Seq.fold (fun s e -> (s * e) % modFactor) 1

let input = "MA" // MA 12 = 3 x stop * 4*A * 1*M

main input
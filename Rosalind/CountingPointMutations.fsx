// http://rosalind.info/problems/hamm/

let src1 = "GAGCCTACTAACGGGAT"
let src2 = "CATCGTAATGACGGCCT"
let answer = 7

Seq.zip src1 src2
|> Seq.filter (fun (s1, s2) -> s1 <> s2)
|> Seq.length


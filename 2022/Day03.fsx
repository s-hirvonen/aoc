open System.Linq
let input = "2022/inputs/day03.txt" |> System.IO.File.ReadAllLines 
let priority = function | ch when int ch > 96 -> int ch - 96 | ch -> int ch - 38

let part1 = 
    input
    |> Seq.map ((Seq.splitInto 2) >> (fun l -> (Seq.head l).Intersect (Seq.last l)) >> Seq.head >> int)
    |> Seq.sumBy priority

let part2 = 
    input
    |> Seq.chunkBySize 3
    |> Seq.map (Seq.map set >> Set.intersectMany >> Set.toList >> List.head >> int)
    |> Seq.sumBy priority

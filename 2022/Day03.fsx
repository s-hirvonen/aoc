open System.Linq
let input = "2022/inputs/day03.txt" |> System.IO.File.ReadAllLines 
let priority = function | ch when ch > 96 -> ch - 96 | ch -> ch - 38

let part1 = Seq.sumBy priority (input
    |> Seq.map (Seq.splitInto 2 >> (fun l -> (Seq.head l).Intersect (Seq.last l)) >> Seq.head >> int))

let part2 = Seq.sumBy priority (input
    |> Seq.chunkBySize 3
    |> Seq.map (Seq.map set >> Set.intersectMany >> Set.toList >> List.head >> int))

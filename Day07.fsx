let input = System.IO.File.ReadAllText "inputs/day07.txt" |> fun s -> s.Split ',' |> Seq.map int |> Seq.toList

let median = input |> List.sort |> (fun s -> (s, List.length s)) |> (fun (s, l) -> s.[l / 2])
let average = input |> Seq.map float |> Seq.average |> int
let part2steps distance = (float distance) / 2.0 * (float distance + 1.0) |> int

let part1 = input |> List.map (fun value -> abs (value - median)) |> Seq.reduce (+)
let part2 = input |> List.map (fun value -> part2steps (abs (value - average))) |> Seq.reduce (+)

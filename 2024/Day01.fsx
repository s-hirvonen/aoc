let input =
    "inputs/day01.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.map (_.Split("  ") >> Seq.map int)
    |> Seq.transpose
    |> Seq.toList

let counts = input[1] |> (Seq.countBy id) |> Map
let part1 = input |> Seq.map Seq.sort |> Seq.transpose |> Seq.sumBy (Seq.reduce (-) >> abs)
let part2 = input[0] |> Seq.sumBy (fun i -> match counts.TryFind i with | Some x -> x * i | None -> 0)

printfn $"{part1} {part2}"

let split (delim: string) (str: string) = str.Split(delim) |> List.ofSeq
let trim (str: string) = str.Trim()

let maxValue (acc: Map<string, int>) (item: string list) =
    acc.Add(item.[1], max (int item.[0]) acc.[item[1]])

let games =
    System.IO.File.ReadAllLines "inputs/day02.txt"
    |> Seq.collect (
        split ":"
        >> List.tail
        >> List.map (
            split ";"
            >> List.collect ((split ",") >> List.map (trim >> split " "))
            >> List.fold maxValue (Map([ "red", 0; "green", 0; "blue", 0 ]))
        )
    )

let part1 =
    games
    |> Seq.indexed
    |> Seq.filter (fun (_, r) -> r.["red"] <= 12 && r.["green"] <= 13 && r.["blue"] <= 14)
    |> Seq.sumBy (fst >> (+) 1)

let part2 = games |> Seq.sumBy (fun r -> r.["red"] * r.["green"] * r.["blue"])

printfn "%d, %d" part1 part2

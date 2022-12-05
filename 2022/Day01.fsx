let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
let input = "2022/inputs/day01.txt" |> System.IO.File.ReadAllText |> split "\n\n" |> Seq.map (split "\n" >> Seq.sumBy int)

// [part1; part2]
[1; 3] |> List.map (fun count -> input |> Seq.sortDescending |> Seq.truncate count |> Seq.reduce (+))

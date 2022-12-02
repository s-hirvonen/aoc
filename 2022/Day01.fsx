let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
let input = "2022/inputs/day01.txt" |> System.IO.File.ReadAllText |> split "\n\n" |> Seq.map (split "\n" >> Seq.sumBy int)

let part1 = input |> Seq.max
let part2 = input |> Seq.sortDescending |> Seq.truncate 3 |> Seq.reduce (+)

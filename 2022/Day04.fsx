open System.Text.RegularExpressions

let split (pattern: string) (str: string) = (Regex pattern).Split(str) |> Array.toList
let fullOverlaps = function
    | [a;b;c;d] when a <= c && b >= d -> 1
    | [a;b;c;d] when a >= c && b <= d -> 1
    | _ -> 0
let overlaps = function
    | [a;b;c;d] when b >= c && a <= d -> 1
    | _ -> 0

let run input check = input |> Seq.sumBy (split "[,-]" >> List.map int >> check)
[fullOverlaps; overlaps] |> List.map (run ("2022/inputs/day04.txt" |> System.IO.File.ReadAllLines))

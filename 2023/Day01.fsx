let input = "inputs/day01.txt" |> System.IO.File.ReadAllLines

let numPosition (defs: string list) (str: string) =
    defs
    |> List.mapi (fun i num -> [ i % 10, str.IndexOf(num); i % 10, str.LastIndexOf(num) ])
    |> List.concat
    |> List.filter (snd >> (<=) 0)

let solve defs =
    input
    |> Seq.sumBy (
        numPosition (defs @ (([0..9] |> List.map string)))
        >> (fun row -> (List.minBy snd row), (List.maxBy snd row))
        >> fun (a, b) -> 10 * (fst a % 10) + (fst b % 10)
    )

let part1 = solve []
let part2 = solve ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] 

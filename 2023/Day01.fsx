let numPosition (defs: string list) (str: string) =
    defs
    |> Seq.mapi (fun i num -> [ i % 10, str.IndexOf(num); i % 10, str.LastIndexOf(num) ])
    |> Seq.concat
    |> Seq.filter (snd >> (<=) 0)

let solve defs =
    "2023/inputs/day01.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.sumBy (
        defs @ ([0..9] |> List.map string) |> numPosition
        >> fun row -> Seq.minBy snd row, Seq.maxBy snd row
        >> fun (a, b) -> fst a % 10 * 10 + fst b % 10
    )

printfn "%d, %d" <|| (solve [], solve ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"])

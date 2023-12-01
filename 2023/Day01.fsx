let nums = ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] @ ([ 0..9 ] |> List.map string)
let numPosition (str: string) =
    nums |> List.map ((str.IndexOf)) |> List.indexed |> List.filter (snd >> (<=) 0)

"inputs/day01.txt"
|> System.IO.File.ReadLines
|> Seq.sumBy (
    numPosition
    >> (fun row -> (List.minBy snd row), (List.maxBy snd row))
    >> fun (a, b) -> 10 * (fst a % 10) + (fst b % 10)
)

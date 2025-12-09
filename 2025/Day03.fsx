let rec greatestNDigitStr n xs =
    if n = 0 then "" else
        let i, d =
            xs
            |> Seq.indexed
            |> Seq.take (Seq.length xs - n + 1)
            |> Seq.maxBy snd

        string d
        + greatestNDigitStr (n - 1) (Seq.skip (i + 1) xs)

let solve n =
    "2025/inputs/day03.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.sumBy (Seq.map string >> greatestNDigitStr n >> int64)

[ 2; 12 ] |> List.map (solve >> printfn "%d")

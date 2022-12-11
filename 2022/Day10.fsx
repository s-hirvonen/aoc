let input =
    "2022/inputs/day10.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.map ((fun s -> s.Split " ") >> List.ofArray)
    |> Seq.scan (fun last command ->
        let regx = List.last last
        match command with
        | "addx"::num -> [regx; regx + int (List.head num)]
        | _           -> [regx]) [1]
    |> Seq.concat

let part1 =
    Seq.indexed
    >> Seq.filter (fun (i, _) -> (i % 40) = 19)
    >> Seq.sumBy (fun (a, b) -> (a+1) * b)

let part2 =
    Seq.mapi (fun cycle regx ->
        match regx - (cycle % 40) + 1 with
        | dx when dx >= 0 && dx < 3 -> "#"
        | _ -> " ")
    >> Seq.chunkBySize 40
    >> Seq.map (Seq.reduce (+))
    >> List.ofSeq

(part1 input, part2 input) |> printfn "%A"

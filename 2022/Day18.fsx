let isAdjacent a b = 2 > (List.zip a b |> List.map (fun (a, b) -> abs (a - b)) |> List.reduce (+))

"2022/inputs/day18.txt"
|> System.IO.File.ReadAllLines
|> Seq.map ((fun (s:string) -> s.Split(",")) >> List.ofSeq >> List.map int) |> List.ofSeq
|> fun l -> l, l |> List.map ((fun i -> List.filter (isAdjacent i) l) >> List.length)
|> snd |> List.sumBy ((-)7)

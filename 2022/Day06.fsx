let solve n = 
    System.IO.File.ReadAllText "2022/inputs/day06.txt"
    |> Seq.windowed n |> Seq.findIndex (Seq.distinct >> Seq.length >> (=)n) |> (+)n

[solve 4; solve 14] |> printfn "%A"

let inline (>=<) a (b,c) = a >= b && a<= c
let input =
    "2022/inputs/day10.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.map ((fun s -> s.Split " ") >> List.ofArray)
    |> Seq.scan (fun last command ->
        let regx = List.last last
        match command with
        | "addx"::num::_ -> [regx; regx + int num]
        | _           -> [regx]) [1]
    |> Seq.concat
    |> Seq.indexed

let part1 =
    Seq.filter (fun (i, _) -> (i % 40) = 19)
    >> Seq.sumBy (fun (a, b) -> (a+1) * b)

let part2 =
    Seq.map ((fun (i, x) -> x - (i % 40) + 1) 
        >> (function | x when x >=< (0,2) -> "#" | _ -> " "))
    >> Seq.chunkBySize 40
    >> Seq.map (Seq.reduce (+))
    >> List.ofSeq

(part1 input, part2 input) |> printfn "%A"

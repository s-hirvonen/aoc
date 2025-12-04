let rotate value move = 
    match move with
    | 'L', x -> value - x
    | _  , x -> value + x
    
let input = 
    "2025/inputs/day01.txt" 
    |> System.IO.File.ReadAllLines
    |> Seq.map (fun s -> s[0], int s[1..])
    |> Seq.scan rotate 50

let part2 = (Seq.pairwise >> Seq.collect (fun (a, b) -> 
    if a < b then seq { a + 1 .. b}
    else seq { a - 1 .. -1 .. b} ))

let solve part = 
    input 
    |> part
    |> Seq.where (fun x -> x % 100 = 0)
    |> Seq.length

printfn @"Part 1: %d, Part 2: %d" (solve id) (solve part2)
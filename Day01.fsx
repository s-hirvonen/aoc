let input = System.IO.File.ReadAllLines "inputs/day01.txt" |> Seq.map int
let greaterThan (a, b) = b > a

// part 1
let part1 input = Seq.pairwise input |> Seq.map greaterThan |> Seq.filter id |> Seq.length
part1 input |> printfn "%i"

// part 2
let input2 = Seq.tail input
Seq.zip3 input input2 (Seq.tail input2) |> Seq.map (fun (a, b, c) -> a + b + c) |> part1 |> printfn "%i"
open System.Linq

type Direction = Up | Down | Left | Right | Low
type Node = { value: int; gradient: Direction; }

let valToNode ch = { value = int ch - int '0'; gradient = Up }
let input = "inputs/day09.txt" |> System.IO.File.ReadAllLines |> Seq.map (Seq.toList >> List.map valToNode) |> Seq.toList

let neigighbors matrix (x, y) =
    let deltas = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] 
    deltas |> List.where (fun (x, y) -> x > 0 && y > 0 && x < matrix.[0].Length && y < matrix.Length)
    

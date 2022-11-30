let input = System.IO.File.ReadAllText "inputs/day06.txt" |> (fun s -> s.Split ',') |> Array.map int
let repeat f n = Seq.init n (fun _ u -> f u) |> Seq.reduce (>>)

let initial = Array.replicate 9 0L
input |> Seq.countBy id |> Seq.iter (fun (num, count) -> initial.[num] <- int64 count)
let nextState (a: int64 array) = [| a.[1]; a.[2]; a.[3]; a.[4]; a.[5]; a.[6]; a.[0] + a.[7]; a.[8]; a.[0] |]

[80; 256] |> List.map (fun days -> repeat nextState days initial |> Array.reduce (+))
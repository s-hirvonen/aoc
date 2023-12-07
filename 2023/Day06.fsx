let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
let replace (a: string) (b: string) (str: string) = str.Replace(a, b)

let solve part =
    "inputs/day06.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (split ":" >> Array.tail >> part)
    |> Array.transpose
    |> Array.map (fun run ->
        Array.map (fun t -> t * (run.[0] - t)) [| 0L .. run.[0] |]
        |> Array.where ((<) run.[1])
        |> Array.length)
    |> Array.reduce (*)

let part1 = Array.collect (split " " >> Array.map int64)
let part2 = Array.map (replace " " "" >> int64)

printfn $"{solve part1}, {solve part2}"

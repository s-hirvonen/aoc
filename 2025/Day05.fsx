let createRanges (lines: string[]) =
    lines
    |> Array.map (_.Split('-') >> fun [|low; high|] -> int64 low, int64 high) 
    |> Array.sortBy fst

let isBetween x (low, high) = x >= low && x <= high
let isFresh ranges ingredient = Seq.exists (isBetween ingredient) ranges

let input = 
    "2025/inputs/day05.txt" 
    |> System.IO.File.ReadAllText 
    |> _.Split("\n\n") 
    |> Seq.map _.Split('\n') 
    |> fun s -> s |> Seq.head |> createRanges, s |> Seq.tail |> Seq.collect (Seq.map int64)

let part1 = input |> fun (ranges, numbers) -> numbers |> Seq.filter (isFresh ranges) |> Seq.length
let part2 = 
    input 
    |> fst
    |> Seq.fold (fun acc curr ->
        match acc with
        | [] -> [curr]
        | (low, high)::rest when fst curr <= high -> (low, max high (snd curr))::rest
        | _ -> curr::acc) []
    |> Seq.sumBy (fun (low, high) -> high - low + 1L)

printfn "Part 1: %d, Part 2: %d" part1 part2
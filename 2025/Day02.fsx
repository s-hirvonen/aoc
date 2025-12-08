let isInvalid (divisor: int) = Seq.splitInto divisor >> Seq.distinct >> Seq.length >> (=) 1

let validate (str: string) =
    seq { 2 .. str.Length }
    |> Seq.where (fun x -> str.Length % x = 0) 
    |> Seq.exists (fun x -> isInvalid x str)

let solve part = 
    "2025/inputs/day02.txt"
    |> System.IO.File.ReadAllText
    |> _.Split(',')
    |> Seq.collect (_.Split('-') >> Array.map int64 >> (fun arr -> seq { arr[0] .. arr[1] }) >> Seq.where (string >> part))
    |> Seq.sum

printfn "Part 1: %d, Part 2: %d" (solve (isInvalid 2)) (solve validate)

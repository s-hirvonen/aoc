let calculate (op, nums) = 
    match op with
    | "*" -> nums |> Seq.map int64 |> Seq.reduce (*)
    | "+" -> nums |> Seq.map int64 |> Seq.reduce (+)
    | _ -> failwith "Invalid input"

let input = "2025/inputs/day06.txt" |> System.IO.File.ReadAllLines

let part1 = 
    input
    |> Seq.map (_.Split(' ') >> Seq.filter (System.String.IsNullOrWhiteSpace >> not))
    |> Seq.transpose 
    |> Seq.sumBy (Seq.rev >> fun row -> calculate (Seq.head row, Seq.tail row))

let part2 =
    input
    |> Array.map Array.ofSeq
    |> (fun arr ->
        arr |> Array.last |> Array.filter (System.Char.IsWhiteSpace >> not) |> Seq.map string,
        Array.sub arr 0 (Array.length arr - 1) 
            |> Array.transpose 
            |> Array.map (System.String >> _.Trim() >> fun s -> s + ".")
            |> System.String.Concat
            |> _.Split("..")
            |> Array.map _.Split('.', System.StringSplitOptions.RemoveEmptyEntries))
    ||> Seq.zip
    |> Seq.sumBy calculate
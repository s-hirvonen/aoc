let scores = Map.ofList [ ')', 3; ']', 57; '}', 1197; '>', 25137 ]
let scores2 = Map.ofList [ ')', 1L; ']', 2L; '}', 3L; '>', 4L ]

let matches opening closing =
    match $"{opening}{closing}" with
    | "()"
    | "[]"
    | "{}"
    | "<>" -> true
    | _ -> false

let parse acc (ch: char) =
    Result.bind
        (fun acc ->
            match ")]}>".Contains ch with
            | false -> Ok(ch :: acc)
            | true ->
                match List.head acc with
                | head when matches head ch -> Ok(List.tail acc)
                | _ -> Error ch)
        acc

let input =
    System.IO.File.ReadAllLines "inputs/day10.txt"
    |> Array.map (List.ofSeq >> (List.fold parse (Ok([]))))

let part1 =
    input
    |> Array.choose (function
        | Error ch -> Some scores.[ch]
        | _ -> None)
    |> Array.sum

let part2 =
    input
    |> Array.choose (function
        | Ok row -> Some row
        | _ -> None)
    |> Array.map (
        List.map (fun ch -> ")]}>".["([{<".IndexOf(string ch)])
        >> List.fold (fun acc ch -> acc * 5L + scores2.[ch]) 0L
    )
    |> Array.sort
    |> fun arr -> arr.[arr.Length / 2]

printfn "%d, %d" part1 part2

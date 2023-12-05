type Mapping = { SourceStart: int64; SourceEnd: int64; Destination: int64 -> int64 }

let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let toMapping =
    function
    | destination :: source :: length :: _ ->
        { SourceStart = source
          SourceEnd = source + length
          Destination = (fun input -> input - (source - destination)) }
    | _ -> failwith "Invalid input"

let input =
    System.IO.File.ReadAllText "inputs/day05.txt"
    |> split "\n\n"
    |> Array.map (split "\n")
    |> List.ofArray
    |> (function
    | (seeds :: maps) ->
        seeds
        |> Array.collect (split ": " >> Array.last >> split " " >> Array.map int64),
        maps |> List.map (Array.tail >> Array.map (split " " >> Array.map int64))
    | _ -> failwith "Invalid input")
    |> (fun (seeds, maps) -> seeds, maps |> List.map (Array.map (List.ofArray >> toMapping)))

let executeMapping (input: int64) (maps: Mapping array) =
    let findMap = Array.tryFind (fun map -> input >= map.SourceStart && input < map.SourceEnd)

    match maps |> findMap with
    | Some mapping -> mapping.Destination input
    | None -> input

let part1 (seeds, maps) = seeds |> Array.map (fun seed -> List.fold executeMapping seed maps) |> Array.min

printfn "%d" (part1 input)

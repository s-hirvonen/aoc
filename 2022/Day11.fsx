open System.Text.RegularExpressions

type Monke = { Items: int64 list; Destinations: int * int; Divider: int64; Test: int64 -> bool; Operation: int64 -> int64; InspectionCount: int64 }
let repeat f n = Seq.init n (fun _ u -> f u) |> Seq.reduce (>>)
let square n = pown n 2
// let kalm n = n / 3L

let rec parse result rest =
    match List.tryFindIndex (fun s-> List.contains s ["Operation";"Test"]) rest with
    | Some index -> parse (rest[..index-1]::result) rest[index+1..]
    | None       -> rest::result

let playRound list =
    list |> fun l -> l, List.indexed l
    ||> List.fold (fun state (i, _) ->
        let commonModulo n = n % (list |> List.map (fun m -> m.Divider) |> Seq.reduce (*))
        let monke = List.item i state
        let outbox =
            monke.Items
            |> List.map (monke.Operation >> commonModulo)
            |> List.partition monke.Test
            ||> fun pass fail -> Map [(fst monke.Destinations, pass); (snd monke.Destinations, fail)]
        let inbox num =
            match outbox |> Map.tryFind num with
            | Some items -> items
            | None -> []

        state |> List.mapi (fun j m -> {
            m with
                Items = if j = i then [] else (m.Items @ inbox j);
                InspectionCount = m.InspectionCount + (if j = i then int64 monke.Items.Length else 0L); }))

"2022/inputs/day11.txt"
|> System.IO.File.ReadAllText
|> fun s -> List.ofArray (s.Split("\n\n"))
|> List.map (fun parts ->
    Regex.Matches(parts, "Operation|Test|[+*]|\d+(?!:)")
    |> Seq.map (fun mc -> mc.Value)
    |> (fun results -> parse [] (List.ofSeq results))
    |> (fun parts -> {
        Items = List.map int64 parts[2]
        Destinations = int parts[0].[1], int parts[0].[2]
        Test = fun level -> level % (int64 parts[0].[0]) = 0
        Divider = int64 parts[0].[0]
        InspectionCount = 0
        Operation =
            match parts[1].[0] with
            | "+" when parts[1].Length > 1 -> (+)(int64 parts[1].[1])
            | "*" when parts[1].Length > 1 -> (*)(int64 parts[1].[1])
            | _   -> square (* new = old * old *) }))
|> repeat playRound 10000 |> List.map (fun m -> m.InspectionCount)
|> List.sortDescending |> List.take 2 |> List.reduce (*)

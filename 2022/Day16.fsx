let split (delim: string[]) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let parse row =
    let rec inner (node, rate, connections) row =
        match row with
        | x::y::xs when x = "Valve" -> inner (y, rate, connections) xs
        | x::y::xs when x = "rate" -> inner (node, int y, connections) xs
        | x::_::xs when x = "to" -> inner (node, rate, xs) []
        | _::xs -> inner (node, rate, connections) xs
        | _ -> (node, (rate, connections))
    inner ("", 0, []) row

let part1 (valves: Map<string, int * string list>) =
    let isOpen valves valve = valves |> Set.contains valve
    let shouldOpenValve (openedValves, current) = not (isOpen openedValves current) && fst valves[current] > 0
    let mutable cache = Map.empty<string * string Set * int, int> // (current, openedValves, time) => solution

    let rec solve state =
        let moveTo (_, openedValves, time, flowRate) destination = solve (destination, openedValves, time - 1, flowRate)
        let openValve (current, openedValves, time, flowRate) =
            solve (current, (Set.add current openedValves), time - 1, flowRate + (fst valves[current]))

        let (current, openedValves, time, flowRate) = state
        // If we have seen this situation before, return precomputed
        match Map.tryFind (current, openedValves, time) cache with
        | Some solution -> solution | None -> (

            match time with
            | 0 -> 0 | _ -> (
                let openOutcome =  if shouldOpenValve (openedValves, current) then [openValve state] else []
                let moveOutcomes = if openOutcome.Length = 0 then (valves[current] |> snd |> List.map (moveTo state)) else []
                let optimal = openOutcome @ moveOutcomes |> Seq.reduce max
                cache <- cache |> Map.add (current, openedValves, time) (optimal + flowRate)
                optimal + flowRate))

    solve ("AA", Set.empty, 30, 0)

"2022/inputs/day16.txt"
|> System.IO.File.ReadAllLines
|> Seq.map (split [|" ";"=";";";","|] >> List.ofArray >> parse)
|> Map.ofSeq
|> part1

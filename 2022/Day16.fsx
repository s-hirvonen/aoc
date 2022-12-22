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

let changeItem fromItem toItem l =
    let index = l |> List.findIndex ((=)fromItem)
    l |> List.insertAt index toItem |> List.removeAt (index + 1)

let solve initialState (valves: Map<string, int * string list>) =
    let shouldOpenValve openedValves current = not (openedValves |> Set.contains current) && fst valves[current] > 0

    let mutable cache = Map.empty<string Set * string Set * int, int> // (current, openedValves, time) => solution
    let mutable bestSoFar = 0

    let rec solve (state: (string list * string Set * int * int)) =
        let (currents, openedValves, time, flowRate) = state
        let moveTo (currents, openedValves, time, flowRate) (from:string) destination = solve (currents |> changeItem from destination, openedValves, time - 1, flowRate)
        let openValve (currents, openedValves, time, flowRate) valveToOpen =
            solve (currents, (Set.add valveToOpen openedValves), time - 1, flowRate + (fst valves[valveToOpen]))

        // If we have seen this situation before, return precomputed
        match Map.tryFind (currents |> Set.ofSeq, openedValves, time) cache with
        | Some solution -> solution | None -> (
            match time with
            | 0 -> 0 | _ -> (
                // For all possible decisions, recurse on all possible outcomes
                let openings = currents |> Seq.map (shouldOpenValve openedValves)

                let outcomes = currents |> List.collect (fun current ->
                    let openOutcome =  if shouldOpenValve openedValves current then [openValve state current] else []
                    let moveOutcomes = if openOutcome.Length = 0 then valves[current] |> snd |> List.map (moveTo state current) else []
                    openOutcome @ moveOutcomes)

                let optimal = outcomes |> Seq.reduce max
                // if optimal > bestSoFar then printfn "%d" optimal
                bestSoFar <- max optimal bestSoFar
                cache <- cache |> Map.add (currents |> Set.ofSeq, openedValves, time) (optimal + flowRate)
                optimal + flowRate))

    solve initialState

"2022/inputs/day16.txt"
|> System.IO.File.ReadAllLines
|> Seq.map (split [|" ";"=";";";","|] >> List.ofArray >> parse)
|> Map.ofSeq
|> solve (["AA"], Set.empty, 30, 0)
// |> solve (["AA"; "AA"], Set.empty, 26, 0)

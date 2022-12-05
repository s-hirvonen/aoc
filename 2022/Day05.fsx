let filteri f = Seq.indexed >> Seq.filter f >> Seq.map (fun (_, x) -> x)
let split (delims: string[]) (s:System.String) = s.Split(delims, System.StringSplitOptions.None)

let solve stackingMode = 
    "2022/inputs/day05.txt"
    |> System.IO.File.ReadAllLines 
    |> Seq.groupBy (fun s -> s.Contains "move") 
    |> Seq.map snd 
    |> (fun input ->
        // stacks
        (Seq.head input 
            |> Seq.transpose
            |> filteri (fun (i, _) -> i % 4 = 1)
            |> Seq.map (Seq.filter (fun s -> int s > 65) >> Seq.toList)
            |> Seq.toList),
        // commands
        Seq.last input
            |> Seq.map (split [|"move "; " from "; " to "|] >> Seq.filter (fun ch -> not (ch.Equals "")) >> Seq.map int >> Seq.toList))
    // execute all commands
    ||> Seq.fold (fun stacks command -> 
        stacks |> List.mapi (fun i x -> 
            match i with
            | num when num + 1 = command.[1] -> List.skip command.[0] x
            | num when num + 1 = command.[2] -> (stacks.[command.[1] - 1] |> List.take command.[0] |> stackingMode) @ x
            | _ -> x))
    // read output
    |> Seq.map Seq.head |> Array.ofSeq |> System.String

[solve List.rev; solve id] |> printfn "%A"

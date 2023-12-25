let hasher = Seq.fold (fun acc ch -> 17 * (acc + int ch) % 256) 0

let (|Remove|Replace|) (str: string) =
    match str.Contains '-' with
    | true -> Remove(str.Remove(str.IndexOf '-'))
    | false -> Replace(str.Split('=') |> fun l -> Array.head l, Array.last l |> int)

let replaceLens l (label, focal) =
    match l |> List.tryFindIndex (fun i -> fst i = label) with
    | Some i -> l[0 .. i - 1] @ [ (label, focal) ] @ l[i + 1 ..]
    | None -> l @ [ (label, focal) ]

let removeLens l label = l |> List.filter (fun i -> fst i <> label)

let input =
    "inputs/day15.txt"
    |> System.IO.File.ReadAllText
    |> _.Split(",", System.StringSplitOptions.TrimEntries)

let part1 = Seq.sumBy hasher

let part2 =
    Seq.fold
        (fun (boxes: (string * int) list array) step ->
            match step with
            | Remove label ->
                boxes[hasher label] <- removeLens boxes[hasher label] label
                boxes
            | Replace(label, focal) ->
                boxes[hasher label] <- replaceLens boxes[hasher label] (label, focal)
                boxes)
        (Array.init 256 (fun _ -> []))
    >> Seq.mapi (fun nBox box -> box |> List.mapi (fun nLens lens -> (1 + nBox) * (1 + nLens) * snd lens))
    >> Seq.collect id
    >> Seq.sum

printfn $"{part1 input}, {part2 input}"

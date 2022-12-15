let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let allPoints ((a, b), (c, d)) = ([min a c..max a c], [min b d..max b d]) ||> Seq.allPairs |> Seq.distinct
let setPoint value arr (a, b) = Array2D.set arr a b value; arr

let draw cave = cave |> Array2D.iteri (fun _ width value ->
    match value with 1 -> printf "█" | 2 -> printf "o" | _ -> printf " "
    if width + 1 = (Array2D.length2 cave) then printf "\n")

let rec solve count cave =
    let rec drop w h (arr: int[,]) =
        match w, h + 1 with
        | w, h when arr.[w,h] = 0 -> drop w h arr
        | w, h when arr.[w - 1, h] = 0 -> drop (w - 1) h arr
        | w, h when arr.[w + 1, h] = 0 -> drop (w + 1) h arr
        | _ -> setPoint 2 arr (w, h)
    try solve (count + 1) (drop 500 0 cave) with
    | :? System.IndexOutOfRangeException -> (cave, count)

"2022/inputs/day14.txt"
|> System.IO.File.ReadAllLines
|> Seq.map ((split " -> ") >> List.ofSeq >> Seq.map ((split ",") >> Array.map int >> fun pair -> pair[0], pair[1]))
|> fun input -> input |> Seq.collect id |> Seq.fold (fun (a, b) (c, d) -> max a c, max b d) (0,0) |> fun (w, h) -> (w+1, h+1) ||> Array2D.zeroCreate<int>, input
|> fun (cave, walls) -> walls |> Seq.collect (Seq.pairwise >> (Seq.collect allPoints) >> Seq.distinct) |> Seq.fold (setPoint 1) cave
|> solve 0 |> fun res -> res |> fst |> draw; snd res

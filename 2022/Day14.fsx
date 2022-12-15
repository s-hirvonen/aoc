let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let offset = 200
let allPoints ((a, b), (c, d)) = ([min a c..max a c], [min b d..max b d]) ||> Seq.allPairs
let setPoint offset value arr (a, b) = Array2D.set arr (a + offset) b value; arr
let addFloor arr = arr |> Array2D.iteri (fun w h _ -> if h = -1 + Array2D.length2 arr then Array2D.set arr w h 1); arr

let rec solve count (cave: int[,]) =
    let rec drop w h (arr: int[,]) =
        match w, h + 1 with
        | w, h when arr.[w,h] = 0 -> drop w h arr
        | w, h when arr.[w - 1, h] = 0 -> drop (w - 1) h arr
        | w, h when arr.[w + 1, h] = 0 -> drop (w + 1) h arr
        | _ -> setPoint 0 2 arr (w, h)
    match cave[500 - offset, 0] with 2 -> count | _ -> solve (count + 1) (drop (500 - offset) 0 cave)

"2022/inputs/day14.txt"
|> System.IO.File.ReadAllLines
|> Seq.map ((split " -> ") >> List.ofSeq >> Seq.map ((split ",") >> Array.map int >> fun pair -> pair[0], pair[1]))
|> fun input -> input |> Seq.collect id |> Seq.fold (fun (a, b) (c, d) -> max a c, max b d) (0,0) |> fun (w, h) -> (w+1 + offset, h+3) ||> Array2D.zeroCreate<int>, input
|> fun (cave, walls) -> walls |> Seq.collect (Seq.pairwise >> (Seq.collect allPoints)) |> Seq.fold (setPoint (0 - offset) 1) cave
|> addFloor |> solve 0

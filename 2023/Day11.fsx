type Node = { X: int64; Y: int64; Value: char }

let findExpansions n (r: char array array) = r.[0 .. n - 1] |> Array.filter (Array.forall ((=) '.')) |> Array.length
let expansionsBefore arrs y x = arrs |> snd |> findExpansions x, arrs |> fst |> findExpansions y

let rec pairs =
    function
    | [] -> []
    | h :: t -> List.map (fun elem -> (h, elem)) t @ pairs t

let input = "inputs/day11.txt" |> System.IO.File.ReadAllLines |> Array.map Seq.toArray
let tInput = input |> Array.transpose
let offset y x pick = expansionsBefore (input, tInput) y x |> pick |> int64

let solve n =
    input
    |> array2D
    |> Array2D.mapi (fun y x value ->
        { X = int64 x + n * offset y x fst; Y = int64 y + n * offset y x snd; Value = value })
    |> Seq.cast<Node>
    |> Seq.filter (_.Value >> (=) '#')
    |> List.ofSeq
    |> pairs
    |> Seq.sumBy (fun (n1, n2) -> abs (n2.Y - n1.Y) + abs (n2.X - n1.X))

printfn $"{solve 1} {solve 999999}"

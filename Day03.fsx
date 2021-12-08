let input = System.IO.File.ReadAllLines "inputs/day03.txt" 
let toNumber str = System.Convert.ToUInt16(str, 2)

let compare tuple = function
| 0 -> (1 + fst tuple, snd tuple)
| _ -> (fst tuple, 1 + snd tuple)

let folder (current: list<(int * int)>) bits = List.mapi (fun i -> compare current.[i]) bits
let initial = Seq.head input |> Seq.toList |> List.map (fun _ -> (0, 0))
let counts = input |> Seq.map (Seq.map (fun c -> int c - int '0') >> Seq.toList) |> Seq.fold folder initial

// part 1
counts
|> Seq.map (function (zero, one) when (zero - one) > 0 -> "0" | _ -> "1")
|> System.String.Concat 
|> (fun s -> (toNumber s, ~~~(toNumber s) <<< 4 >>> 4))
|> fun (gamma, epsilon) -> int gamma * int epsilon |> printfn "%i"

// part 2
type BitCriteria = O2 | CO2

let selector = function
| O2 -> Seq.sortByDescending fst >> Seq.maxBy snd
| CO2 -> Seq.sortBy fst >> Seq.minBy snd

let rec solution criteria i list = 
    match list with
    | [| result |] -> System.Convert.ToInt32 (result, 2)
    | _ -> 
        let bit = list |> Seq.transpose |> Seq.item i |> Seq.countBy id |> selector criteria |> fst
        let ans = list |> Array.where (fun bits -> bits.[i] = bit)
        solution criteria (i + 1) ans

[|solution O2 0 input; solution CO2 0 input|] |> Seq.reduce (*)
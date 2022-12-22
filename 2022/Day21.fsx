type Equation = Constant of decimal | Operation of (decimal -> decimal-> decimal) * string * string

let split (delim:string) (str:string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries) |> Seq.toList
let operation = function "+" -> (+) | "-" -> (-) | "*" -> (*) | _ -> (/)

let parse = function
| monke::eq::_ -> (monke,
    match split " " eq with
    | [num] -> Constant (decimal num)
    | eq -> Operation (operation eq[1], eq[0], eq[2]))
| _ -> failwith "Invalid row"

let rec value (dict: Map<string, Equation>) = function
| Constant c -> c
| Operation (op, lhs, rhs) -> op (value dict dict[lhs]) (value dict dict[rhs])

let lookup dict str = value dict dict[str]
let rec bisect dict floor exp =
    let nextDict = dict |> Map.add "humn" (Constant (floor + (pown 2m (exp + 1))))
    match (lookup nextDict "bsgz" - lookup nextDict "lsbv") with
    | x when x < 0m -> bisect nextDict floor (exp + 1)
    | x when x > 0m -> bisect dict (floor + pown 2m exp) 0
    | _ -> nextDict["humn"]

"2022/inputs/day21.txt"
|> System.IO.File.ReadAllLines
|> Seq.map (split ":" >> parse)
|> Map.ofSeq
|> fun dict -> lookup dict "root", bisect (dict |> Map.add "humn" (Constant 1m)) 0m 0

type Point =
    | Number of int
    | Symbol of char

type Part = { Value: Point; X: int; Y: int }

let inline (>=<) low high x = x >= low && x <= high

let value =
    function
    | { Value = Number x } -> x
    | { Value = Symbol _ } -> failwith "Symbol has no value"

let newState (items, reading) (x, y, ch) =
    match ch with
    | ch when System.Char.IsDigit ch ->
        match reading, x with
        | false, _
        | true, 0 -> [ { Value = Number(int ch - int '0'); X = x; Y = y } ] @ items, true
        | true, _ ->
            [ { List.head items with
                  Value = Number(items |> List.head |> value |> (*) 10 |> (+) (int ch - int '0')) } ]
            @ List.tail items,
            true
    | '.' -> items, false
    | ch -> [ { Value = Symbol ch; X = x; Y = y } ] @ items, false

let hasAdjacentSymbol items point =
    match point.Value with
    | Number n ->
        items
        |> List.exists (fun candidate ->
            match candidate.Value with
            | Symbol _ ->
                (>=<) (point.Y - 1) (point.Y + 1) (candidate.Y)
                && (>=<) (point.X - 1) (point.X + (n |> string |> Seq.length)) (candidate.X)
            | Number _ -> false)
    | _ -> false // This is a symbol point, not part of schematic

let gearRatio items point =
    items
    |> List.filter (fun candidate ->
        match candidate.Value with
        | Symbol _ -> false
        | Number n ->
            (>=<) (point.Y - 1) (point.Y + 1) (candidate.Y)
            && (>=<) (point.X - (n |> string |> Seq.length)) (point.X + 1) (candidate.X))


let input =
    System.IO.File.ReadAllLines "inputs/day03.txt"
    |> array2D
    |> Array2D.mapi (fun y x value -> (x, y, value))
    |> Seq.cast<int * int * char>
    |> Seq.fold newState ([], false)
    |> fst

let part1 =
    input
    |> fun items -> (items |> List.filter (hasAdjacentSymbol items))
    |> List.sumBy value

let part2 =
    input
    |> fun items ->
        (items
         |> List.filter (function
             | { Value = Symbol '*' } -> true
             | _ -> false))
        |> List.map (gearRatio items)
    |> List.filter (List.length >> (=) 2)
    |> List.sumBy (List.map value >> List.reduce (*))

printfn "%d, %d" part1 part2

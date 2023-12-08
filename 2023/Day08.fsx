let (path, nodes) =
    "inputs/day08.txt"
    |> System.IO.File.ReadAllLines
    |> fun i -> Seq.head i, i |> Seq.tail |> Seq.tail
    |> fun (path, nodes) -> path, nodes |> Seq.map (fun str -> str[..2], (str[7..9], str[12..14])) |> dict

let rec lcm (nums: int64 seq) =
    let rec gcd a b = if b = 0L then abs a else gcd b (a % b)

    match nums |> List.ofSeq with
    | [ a; b ] -> a * b / (gcd a b)
    | head :: tail -> head * lcm tail / (gcd head (lcm tail))
    | [] -> 1

let solve endCondition startItem =
    let branch dir = if dir = 'L' then fst else snd

    let rec step steps current =
        match endCondition current with
        | true -> steps
        | false -> step (steps + 1) (nodes.Item current |> branch path.[steps % path.Length])

    step 0 startItem

let part1 = solve (_.Equals("ZZZ")) "AAA"

let part2 =
    nodes
    |> Seq.map _.Key
    |> Seq.filter _.EndsWith("A")
    |> Seq.map (solve (_.EndsWith("Z")) >> int64)
    |> lcm

printfn $"{part1}, {part2}"

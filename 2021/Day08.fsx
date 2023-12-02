let input =
    System.IO.File.ReadAllLines "inputs/day08.txt"
    |> Seq.map (fun s ->
        s.Split " | "
        |> Array.map ((fun s -> s.Split " "))
        |> fun r -> Seq.head r |> Seq.map set |> set, Seq.last r)

let rec insertions x =
    function
    | [] -> [ [ x ] ]
    | (y :: ys) as l -> (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))

let rec permutations =
    function
    | [] -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let digits =
    [ "abcefg"; "cf"; "acdeg"; "acdfg"; "bcdf"; "abdfg"; "abdefg"; "acf"; "abcdefg"; "abcdfg" ]

let digitsForPermutation (key: string) =
    digits
    |> List.map (Seq.map (fun ch -> key.["abcdefg".IndexOf ch]) >> set)
    |> set,
    key

let possibleCombinations =
    [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ]
    |> permutations
    |> Seq.map (Array.ofList >> System.String >> digitsForPermutation)
    |> List.ofSeq

let part1 =
    input
    |> Seq.collect (
        snd
        >> Array.map String.length
        >> Array.filter (fun l -> List.exists (fun i -> i = l) [ 2; 4; 3; 7 ])
    )
    |> Seq.length

let part2 =
    input
    |> Seq.sumBy (
        (fun (definitions, row) ->
            possibleCombinations
            |> Seq.find (fun (combi, _) -> definitions.IsSubsetOf combi)
            |> snd
            |> fun key ->
                row
                |> Array.map (
                    Seq.map (fun ch -> "abcdefg".[key.IndexOf ch])
                    >> Seq.sort
                    >> Array.ofSeq
                    >> System.String
                    >> fun d -> List.findIndex ((=) d) digits
                    >> string
                )
            |> String.concat ""
            |> int)
    )

printfn "%d, %d" part1 part2

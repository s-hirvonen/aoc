let input =
    "2025/inputs/day04.txt"
    |> System.IO.File.ReadAllLines
    |> array2D

let neighbors arr =
    arr
    |> Array2D.mapi (fun i j ch ->
        [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
        |> List.choose (fun (di, dj) ->
            try Some(Array2D.get arr (i + di) (j + dj))
            with | :? System.IndexOutOfRangeException -> None)
        |> List.where ((=) '@')
        |> List.length, ch)

let extract =
    neighbors
    >> Array2D.map (function
        | count, _ when count < 4 -> '.'
        | _, ch -> ch)

let countAccessible =
    neighbors
    >> Seq.cast<int * char>
    >> Seq.where (fun (n, ch) -> ch = '@' && n < 4)
    >> Seq.length

let rec extractAll totalCount arr =
    match countAccessible arr with
    | 0 -> totalCount
    | n -> extractAll (totalCount + n) (extract arr)

let part1 = input |> countAccessible
let part2 = input |> extractAll 0

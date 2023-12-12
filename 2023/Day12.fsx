open System.Collections.Generic

let solveRow pattern nums =
    let rec score p n current (memo: Dictionary<(int * int * int), int64>) =
        if memo.ContainsKey(p, n, current) then
            memo.Item(p, n, current)

        elif p = String.length pattern then
            if n = Array.length nums && current = 0 then 1L
            elif n + 1 = Array.length nums && nums.[n] = current then 1L
            else 0L
        else
            let mutable ans = 0L

            [ '#'; '.' ]
            |> List.iter (fun ch ->
                if pattern.[p] = ch || pattern.[p] = '?' then
                    match ch, current with
                    | '.', 0 -> ans <- ans + (score (p + 1) n 0 memo)
                    | '.', _ ->
                        if n < Array.length nums && nums.[n] = current then
                            ans <- ans + (score (p + 1) (n + 1) 0 memo)
                    | _ -> ans <- ans + (score (p + 1) n (current + 1) memo))

            memo.Add((p, n, current), ans)
            ans

    score 0 0 0 (Dictionary<(int * int * int), int64>())

let solve transform =
    "inputs/day12.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (_.Split(" ") >> fun a -> a[0], a[1].Split(",") |> Array.map int)
    |> Array.sumBy (transform >> (<||) solveRow)

let part1 = solve id
let part2 = solve (fun (p, n) -> String.concat "?" [ p; p; p; p; p ], Array.concat [ n; n; n; n; n ])

printfn $"{part1}, {part2}"

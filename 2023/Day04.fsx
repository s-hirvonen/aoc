type Item = { Index: int; Wins: int; Owned: int }

let split (delim: string) (str: string) =
    str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let input =
    System.IO.File.ReadAllLines "inputs/day04.txt"
    |> Array.map (
        split ":"
        >> Array.last
        >> split " | "
        >> Array.map (split " " >> Array.map int >> set)
        >> Array.reduce Set.intersect
        >> Set.count
    )

let folder (state: Item array) item =
    [ item.Index + 1 .. item.Index + item.Wins ]
    |> List.iter (fun n -> state.[n] <- { state.[n] with Owned = state.[n].Owned + state.[item.Index].Owned })

    state

let part1 = input |> Array.sumBy ((+) -1 >> pown 2)

let part2 =
    input
    |> Array.mapi (fun i n -> { Index = i; Wins = n; Owned = 1 })
    |> (fun state -> Array.fold folder state state)
    |> Array.sumBy (fun i -> i.Owned)

printfn "%d, %d" part1 part2

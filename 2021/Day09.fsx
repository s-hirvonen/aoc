let neighborCoords (input: 'a[,]) y x =
    [ if y > 0 then
          yield (x, y - 1)
      if y < Array2D.length1 input - 1 then
          yield (x, y + 1)
      if x > 0 then
          yield (x - 1, y)
      if x < Array2D.length2 input - 1 then
          yield (x + 1, y) ]

let neighbors r c (A: 'a[,]) =
    neighborCoords A r c |> List.map (fun (x, y) -> A.[y, x])

let localBottom arr y x =
    (neighbors y x arr) |> List.forall (fun node -> node > (Array2D.get arr y x))

let rec mergeIntersectingSets (sets: Set<'a> list) =
    let rec merge acc remainingSets =
        match remainingSets with
        | [] -> acc
        | set :: rest ->
            let nonOverlappingSets, overlappingSets =
                List.partition (fun s -> Set.isEmpty (Set.intersect set s)) rest

            let mergedSet = Set.union set (Set.unionMany overlappingSets)
            merge (mergedSet :: acc) nonOverlappingSets

    let mergedSets = merge [] sets

    if List.length mergedSets < List.length sets then
        mergeIntersectingSets mergedSets
    else
        mergedSets

let input =
    "inputs/day09.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.map (Seq.map (fun ch -> int ch - int '0'))
    |> array2D

let notTop (x, y) = input.[y, x] <> 9

let part1 =
    input
    |> Array2D.mapi (fun y x n -> (n, localBottom input y x))
    |> Seq.cast<(int * bool)>
    |> Seq.filter snd
    |> Seq.sumBy (fst >> (+) 1)

let part2 =
    input
    |> Array2D.mapi (fun y x _ -> (x, y), neighborCoords input y x)
    |> Seq.cast<((int * int) * (int * int) list)>
    |> Seq.filter (fst >> notTop)
    |> Seq.fold
        (fun basins ((x, y), neighbors) -> List.append basins [ ((x, y) :: (neighbors |> List.filter notTop) |> Set) ])
        List.empty<(int * int) Set>
    |> mergeIntersectingSets
    |> List.map Set.count
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

printfn "%d, %d" part1 part2

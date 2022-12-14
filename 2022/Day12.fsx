type Node = { Value: char; X: int; Y: int; Next: (int * int) list; }

let inline (><) a (b,c) = a > b && a < c
let exists maze (y, x) = y >< (-1, Array2D.length1 maze) && x >< (-1, Array2D.length2 maze)
let add a b = fst a + fst b, snd a + snd b
let neighbors maze point = [-1,0; 0,-1; 0,1; 1,0] |> List.map (add point) |> List.filter (exists maze)
let distance n1 n2 = (pown (n2.X - n1.X) 2) + (pown (n2.Y - n1.Y) 2) |> float |> sqrt |> int

let pathfind maze goal start =
    let h = distance goal
    let rec reconstruct (cameFrom: Map<Node, Node>) (current: Node) (path: Node list) =
        match cameFrom |> Map.tryFind current with
        | Some from -> reconstruct cameFrom from ([current] @ path)
        | None   -> path

    let rec inner current openSet cameFrom (gScore: Map<Node, int>) (fScore: Map<Node, int>) neighbors =
        match neighbors with
        | [] -> (openSet, cameFrom, gScore, fScore)
        | (y, x)::xs -> (
            let tentative_gScore = gScore.[current] + 1
            let neighbor = Array2D.get maze y x
            let neighbor_gScore = Map.tryFind neighbor gScore |> function | Some a -> a | None -> System.Int32.MaxValue
            if tentative_gScore < neighbor_gScore then
                inner
                    current (openSet |> Set.add neighbor)
                    (cameFrom |> Map.add neighbor current)
                    (gScore |> Map.add neighbor tentative_gScore)
                    (fScore |> Map.add neighbor (tentative_gScore + h neighbor))
                    xs
            else inner current openSet cameFrom gScore fScore xs)

    let rec traverse openSet cameFrom (gScore: Map<Node, int>) (fScore: Map<Node, int>) =
        try match (openSet |> Seq.minBy (fun node -> fScore.[node])) with
            | n when n = goal -> Some (reconstruct cameFrom goal [])
            | current -> (
                let (os, cf, gs, fs) = inner current (openSet |> Set.remove current) cameFrom gScore fScore current.Next
                traverse os cf gs fs)
        with | :? System.ArgumentException -> None

    traverse (Set [start]) (Map.empty) ([(start, 0)] |> Map.ofSeq) ([(start, h start)] |> Map.ofSeq)

System.IO.File.ReadAllLines "2022/inputs/day12.txt" |> array2D
|> fun input ->
    let canStepTo fromPoint toPoint =
        let fromValue = fromPoint ||> Array2D.get input |> (function | 'S' -> 'a' | 'E' -> 'z' | ch -> ch)
        let toValue   = toPoint   ||> Array2D.get input |> (function | 'S' -> 'a' | 'E' -> 'z' | ch -> ch)
        int toValue - int fromValue <= 1

    let maze = input |> Array2D.mapi (
        (fun y x ch -> { Value = ch; X = x; Y = y; Next = ((y, x) |> neighbors input |> List.filter (canStepTo (y, x))) }))
    let find ch = maze |> Seq.cast<Node> |> Seq.find (fun n -> n.Value = ch)

    let part1 = [pathfind maze (find 'E') (find 'S')] |> Seq.choose id

    let part2 =
        maze |> Seq.cast<Node> |> Seq.filter (fun n -> n.Value = 'a')
        |> Seq.map (pathfind maze (find 'E')) |> Seq.choose id

    let draw path =
        input |> Array2D.iteri (fun y x ch ->
            match (path |> List.tryFind (fun node -> node.X = x && node.Y = y)) with
            | Some _ -> printf "█"
            | None   -> printf "%c" ch
            if x + 1 = (Array2D.length2 input) then printf "\n")

    part1 |> Seq.iter draw
    [part1; part2] |> Seq.map ((Seq.minBy Seq.length) >> Seq.length) |> printfn "%A"

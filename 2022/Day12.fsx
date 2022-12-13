type Node = { Value: char; X: int; Y: int; Next: (int * int) list; }

let inline (><) a (b,c) = a > b && a < c
let exists maze (y, x) = y >< (-1, Array2D.length1 maze) && x >< (-1, Array2D.length2 maze)
let add a b = fst a + fst b, snd a + snd b
let neighbors maze point = [-1,0; 0,-1; 0,1; 1,0] |> List.map (add point) |> List.filter (exists maze)
let distance n1 n2 = (pown (n2.X - n1.X) 2) + (pown (n2.Y - n1.Y) 2) |> float |> sqrt |> int

let input = System.IO.File.ReadAllLines "2022/inputs/day12.txt" |> array2D
let canStepTo fromPoint toPoint =
    let fromValue = fromPoint ||> Array2D.get input |> (function | 'S' -> 'a' | 'E' -> 'z' | ch -> ch)
    let toValue   = toPoint   ||> Array2D.get input |> (function | 'S' -> 'a' | 'E' -> 'z' | ch -> ch)
    int toValue - int fromValue <= 1

let maze = input |> Array2D.mapi (
    (fun y x ch -> { Value = ch; X = x; Y = y; Next = ((y, x) |> neighbors input |> List.filter (canStepTo (y, x))) }))

let pathfind goal start =
    let h = distance goal
    let rec reconstruct (cameFrom: Map<Node, Node>) (current: Node) (path: Node list) =
        match cameFrom |> Map.tryFind current with
        | Some from -> reconstruct cameFrom from ([current] @ path)
        | None   -> path

    let rec traverse current openSet cameFrom (gScore: Map<Node, int>) (fScore: Map<Node, int>) remainingNeighbors =
        let isLooping    = Seq.length remainingNeighbors > 0
        if Set.isEmpty openSet && not isLooping then None else // No path found
        let current      = if isLooping then current else openSet |> Seq.minBy (fun node -> fScore.[node])
        let newOpenSet   = if isLooping then openSet else openSet |> Set.remove current
        let newNeighbors = if isLooping then remainingNeighbors else current.Next

        match current with
        | n when n = goal -> Some (reconstruct cameFrom current []) // Path found, reconstruct and return
        | _ ->
            match newNeighbors with
            | [] -> traverse current newOpenSet cameFrom gScore fScore newNeighbors
            | (y, x)::xs -> (
                let tentative_gScore = gScore.[current] + 1
                let neighbor = Array2D.get maze y x
                let neighbor_gScore = Map.tryFind neighbor gScore |> function | Some a -> a | None -> System.Int32.MaxValue
                if tentative_gScore < neighbor_gScore then
                    traverse
                        current
                        (newOpenSet |> Set.add neighbor)
                        (cameFrom |> Map.add neighbor current)
                        (gScore |> Map.add neighbor tentative_gScore)
                        (fScore |> Map.add neighbor (tentative_gScore + h neighbor))
                        xs
                else traverse current newOpenSet cameFrom gScore fScore xs)

    let gScore = [(start, 0)] |> Map.ofSeq
    let fScore = [(start, h start)] |> Map.ofSeq
    traverse start (Set [start]) (Map.empty) gScore fScore []

let part1 =
    let start = maze |> Seq.cast<Node> |> Seq.find (fun n -> n.Value = 'S')
    let goal  = maze |> Seq.cast<Node> |> Seq.find (fun n -> n.Value = 'E')
    pathfind goal start |> function | Some path -> Seq.length path | None -> failwith "No path found"

let part2 =
    let goal =  maze |> Seq.cast<Node> |> Seq.find (fun n -> n.Value = 'E')
    maze |> Seq.cast<Node> |> Seq.filter (fun node -> node.Value = 'a')
    |> Seq.map (pathfind goal) |> Seq.choose id |> Seq.minBy Seq.length |> Seq.length

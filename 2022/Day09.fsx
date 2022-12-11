let add a b = fst a + fst b, snd a + snd b
let sub a b = fst a - fst b, snd a - snd b
let Abs a   = abs (fst a), abs (snd a)
let initWithValue count value = List.init count (fun _ -> value)

let parse (visited, head, tails) command =
    let newHead = function
    | "L" -> add head (-1, 0)
    | "R" -> add head (1, 0)
    | "U" -> add head (0, 1)
    |  _  -> add head (0, -1)

    let newTail tail = function
    | (dx, dy) when Abs (dx, dy) = (2, 0) -> add tail (dx/2, 0   )
    | (dx, dy) when Abs (dx, dy) = (0, 2) -> add tail (0   , dy/2)
    | (dx, dy) when Abs (dx, dy) = (2, 1) -> add tail (dx/2, dy  )
    | (dx, dy) when Abs (dx, dy) = (1, 2) -> add tail (dx  , dy/2)
    | (dx, dy) when Abs (dx, dy) = (2, 2) -> add tail (dx/2, dy/2)
    | _ -> tail

    let newTails = (newHead command, tails) ||> List.scan (fun head tail -> newTail tail (sub head tail)) 
    Set.add (List.last newTails) visited, List.head newTails, List.tail newTails

let solve length = 
    "2022/inputs/day09.txt" 
    |> System.IO.File.ReadAllLines 
    |> Seq.map ((fun s -> s.Split(" ")) >> (fun row -> initWithValue (int row.[1]) (row.[0])))
    |> Seq.concat
    |> Seq.fold parse (Set.empty<int * int>, (0,0), initWithValue length (0,0))
    |> fun (state, _, _) -> Set.count state

(solve 1, solve 9)

let add a b = fst a + fst b, snd a + snd b
let sub a b = fst a - fst b, snd a - snd b
let initWithValue count value = List.init count (fun _ -> value)

let newHead = function
| "L" -> add (-1, 0) | "R" -> add (1,  0)
| "U" -> add (0 , 1) |  _  -> add (0, -1)

let newTail = function
| (dx, dy) when abs dx = 2 && abs dy = 2 -> add (dx/2, dy/2)
| (dx, dy) when abs dx = 2               -> add (dx/2, dy  )
| (dx, dy) when abs dy = 2               -> add (dx  , dy/2)
| _ -> id

let parse (visited, head, tails) command =
    let newTails = (newHead command head, tails) ||> List.scan (fun h t -> newTail (sub h t) t)
    Set.add (List.last newTails) visited, List.head newTails, List.tail newTails

let solve length = 
    System.IO.File.ReadAllLines "2022/inputs/day09.txt"
    |> Seq.collect ((fun s -> s.Split(" ")) >> (fun row -> initWithValue (int row.[1]) (row.[0])))
    |> Seq.fold parse (Set.empty<int * int>, (0,0), initWithValue length (0,0))
    |> fun (state, _, _) -> Set.count state

[1; 9] |> List.map solve

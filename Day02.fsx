let input =
    System.IO.File.ReadAllLines "inputs/day02.txt" 
    |> Seq.map ((fun s -> s.Split ' ') >> (fun l -> (l.[0], int l.[1])))

let move (pos, depth, aim) = function
| ("forward", x) -> (pos + x, depth + (x * aim), aim)
| ("down", x) -> (pos, depth, aim + x)
| (_, x) -> (pos, depth, aim - x)

let part1 = Seq.fold move (0, 0, 0) input |> (fun (pos, _, depth) -> pos * depth)
let part2 = Seq.fold move (0, 0, 0) input |> (fun (pos, depth, _) -> pos * depth)
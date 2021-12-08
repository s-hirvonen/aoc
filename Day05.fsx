let splitter (s: string) = s.Split([|","; " -> "|], System.StringSplitOptions.RemoveEmptyEntries)
let input = 
    System.IO.File.ReadAllLines "inputs/day05.txt" 
    |> Array.map (splitter >> fun (a) -> [|(int a.[0], int a.[1]); (int a.[2], int a.[3])|] |> Array.sortBy snd |> Array.sortBy fst)

let isOrthogonal = function | [|(a, b); (c, d)|] when a = c || b = d -> true | _ -> false

let drawLine = function
| [|(x1, y1); (x2, y2)|] when y1 = y2 && x1 < x2 -> ((x1, x2) |> Array.unfold (fun (c, max) -> if c > max then None else Some ((c, y1), (c + 1, max))))
| [|(x1, y1); (x2, y2)|] when x1 = x2 && y1 < y2 -> ((y1, y2) |> Array.unfold (fun (c, max) -> if c > max then None else Some ((x1, c), (c + 1, max))))
| [|(x1, y1); (x2, y2)|] when x1 <> x2 && y1 < y2 -> (((x1, y1), y2) |> Array.unfold (fun ((x,y), max) -> if y > max then None else Some ((x, y), ((x+1,y+1), max))))
| [|(x1, y1); (x2, y2)|] when x1 <> x2 && y1 > y2 -> (((x1, y1), y2) |> Array.unfold (fun ((x,y), max) -> if y < max then None else Some ((x, y), ((x+1,y-1), max))))
| _ -> [||]


let solution grid = grid |> Array.map drawLine |> Array.concat |> Array.countBy id |> Array.map snd |> Array.where (fun c -> c > 1) |> Array.length
let part1 = input |> Array.where isOrthogonal |> solution
let part2 = input |> solution


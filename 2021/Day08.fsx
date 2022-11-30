let input = System.IO.File.ReadAllLines "inputs/day08.txt" |> Seq.map (fun s -> s.Split " | " |> Array.toList |> List.map (fun s -> s.Split " "))

let part1 = input |> Seq.map (List.item 1 >> Seq.where (fun s -> [2;3;4;7] |> List.exists (fun l -> l = s.Length))) |> Seq.concat |> Seq.length


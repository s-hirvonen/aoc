open Newtonsoft.Json.Linq

"2022/inputs/day13.txt" |> System.IO.File.ReadAllText |> (fun (s:string) -> s.Split("\n\n"))

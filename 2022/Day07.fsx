type Node = { Content: string * int; Children: Node list }
let removeLast = List.rev >> List.tail >> List.rev

let rec insert = function
| ([], n) -> n
| (x::xs, []) -> [{Content = x; Children = insert (xs, [])}]
| (x::xs, {Content = nm; Children = ch}::ns) ->
    if nm = x then {Content = nm; Children = insert (xs, ch)}::ns
    else           {Content = nm; Children = ch}::insert (x::xs, ns)

let rec setSizes = function
| ({ Content = nm; Children = []}) -> { Content = nm; Children = [] }
| dir -> 
    let childrenSize = dir.Children |> List.map setSizes |> List.map (fun node -> snd node.Content) |> List.reduce (+)
    { Content = (fst dir.Content, childrenSize); Children = dir.Children |> List.map setSizes }

let rec dirSizes = function
| ({ Content = _; Children = []}) -> []
| dir -> [snd dir.Content] @ (dir.Children |> List.collect dirSizes)

let parse (filesystem, cwd) = function
| "$" :: "cd" :: dir -> 
    match dir with
        | [".."] -> filesystem, removeLast cwd
        | dir    -> filesystem, cwd @ [(dir.[0], 0)]
| "$" :: "ls" :: _  -> filesystem, cwd
| "dir" :: [dir]    -> insert ((cwd @ [(dir, 0)]), filesystem), cwd
| size :: name :: _ -> insert ((cwd @ [name, int size]), filesystem), cwd
| _                 -> filesystem, cwd

let input =
    System.IO.File.ReadAllLines "2022/inputs/day07.txt"
    |> Seq.map ((fun s -> s.Split " ") >> Array.toList)
    |> Seq.fold parse (insert ([], []), []) |> fst |> Seq.head |> setSizes |> dirSizes

input |> List.filter ((>)100000) |> List.reduce (+),
input |> List.sortDescending |> fun sizes -> sizes |> List.findBack ((<)(30000000 - 70000000 + (sizes |> List.head)))

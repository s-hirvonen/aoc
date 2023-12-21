let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
let minLength = List.minBy Array.length >> Array.length
let findIndices fn = Seq.indexed >> Seq.filter fn >> Seq.map fst >> List.ofSeq

let findReflections (arr: char array array) =
    [ for i in 1 .. arr.Length - 1 do
          [ Array.rev arr.[.. i - 1]; arr.[i..] ] ]
    |> Seq.map (fun parts -> (parts |> List.map (parts |> minLength |> Array.truncate)))
    |> findIndices (snd >> set >> Seq.length >> (=) 1)
    |> List.map ((+) 1)

let variants (arr: char array array) =
    seq {
        for i in 0 .. arr.Length - 1 do
            for j in 0 .. arr[i].Length - 1 do
                let newArr = Array.copy arr
                newArr[i] <- Array.copy arr[i]
                newArr[i][j] <- if arr[i][j] = '.' then '#' else '.'
                yield newArr
    }

let solve =
    (fun grid -> [ grid; Array.transpose grid ])
    >> List.map findReflections
    >> (function
    | [ a; b ] -> a |> List.map ((*) 100) |> List.append b
    | _ -> failwith "Could not find reflection")

let input =
    "inputs/day13.txt"
    |> System.IO.File.ReadAllText
    |> split "\n\n"
    |> Array.map (split "\n" >> Array.map Array.ofSeq >> (fun grid -> grid, solve grid))

let part1 = input |> Seq.collect snd |> Seq.sum

let part2 =
    input
    |> Seq.collect (fun (grid, ans) -> grid |> variants |> Seq.collect solve |> set |> Seq.except ans)
    |> Seq.sum

printfn $"{part1}, {part2}"

let bingo = "inputs/day04.txt" |> System.IO.File.ReadLines |> Seq.head |> fun s -> s.Split ',' |> Array.map int |> Array.toList

// key: bingoNumber, value: order in game
let dict = bingo |> List.mapi (fun s i -> (i, s)) |> Map
let toSquare = fun i -> (i, dict.[i])

let boards = 
    "inputs/day04.txt" |> System.IO.File.ReadAllText |> fun s -> s.Split "\n\n" |> Array.skip 1
    |> Array.map (fun s -> 
        s.Split ([|' '; '\n'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (int >> toSquare)
        |> fun row -> 
            let horizontal = row |> Array.chunkBySize 5
            let vertical = horizontal |> Array.transpose
            Array.append horizontal vertical)

let lineWinsOnTurn line = line |> Array.map snd |> Array.max
let boardWinsOnTurn board = board |> Array.map lineWinsOnTurn |> Array.min |> fun turn -> (turn, board)
let solution (turn, board) =
    let squares = Array.take 5 board |> Array.concat
    let winningNumber = squares |> Array.where (fun (_, t) -> t = turn) |> Array.head |> fst
    squares
    |> Array.where (fun (_, turnDrawn) -> turnDrawn > turn) // unmarked numbers
    |> Array.map fst
    |> Array.reduce (+)
    |> (*) winningNumber

let part1 = boards |> Array.map boardWinsOnTurn |> Array.minBy fst |> solution
let part2 = boards |> Array.map boardWinsOnTurn |> Array.maxBy fst |> solution
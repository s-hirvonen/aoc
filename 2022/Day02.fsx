open LanguagePrimitives

// I said the real modulo operator
let inline (%!) a b = (a % b + b) % b
let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
let v a = EnumToValue a
type Choice = Rock = 0 | Paper = 1 | Scissors = 2
type Result = Draw = 0 | PlayerWin = 1 | OpponentWin = 2

let toChoice = function
    | "A" | "X" -> Choice.Rock
    | "B" | "Y" -> Choice.Paper
    | "C" | "Z" | _ -> Choice.Scissors

let result (player: Choice) (opponent: Choice): Result = EnumOfValue ((v player - v opponent) %! 3)
let choiceFromResult (opponent: Choice) (result: Result): Choice = EnumOfValue ((v opponent + v result) %! 3)
let score ([|opponent; player|]) =
    match (result player opponent) with
    | Result.PlayerWin -> 6 + v player + 1
    | Result.OpponentWin -> 0 + v player + 1
    | _ -> 3 + v player + 1

let toResult = function
    | "X" -> Result.OpponentWin
    | "Y" -> Result.Draw
    | "Z" | _ -> Result.PlayerWin


let input = "2022/inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map (split " ")
let part1 = input |> Seq.map (Array.map toChoice >> score) |> Seq.reduce (+)
let part2 = input |> Seq.map ((fun row -> [| toChoice row.[0]; choiceFromResult (toChoice row.[0]) (toResult row.[1])|]) >> score) |> Seq.reduce (+)

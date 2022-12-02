// I said the real modulo operator
let inline (%!) a b = (a % b + b) % b
let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
type Choice = Rock = 0 | Paper = 1 | Scissors = 2
type Result = Draw = 0 | PlayerWin = 1 | OpponentWin = 2

let toChoice = function
    | "A" | "X" -> int Choice.Rock
    | "B" | "Y" -> int Choice.Paper
    | "C" | "Z" | _ -> int Choice.Scissors

let result player opponent = (player - opponent) %! 3
let choiceFromResult opponent result = (opponent + result) %! 3
let score (opponent, player) =
    match LanguagePrimitives.EnumOfValue (result player opponent) with
    | Result.PlayerWin -> 6 + player + 1
    | Result.OpponentWin -> 0 + player + 1
    | Result.Draw | _ -> 3 + player + 1

let toResult = function
    | "X" -> int Result.OpponentWin
    | "Y" -> int Result.Draw
    | "Z" | _ -> int Result.PlayerWin

let input = "2022/inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map (split " ")
let part1 = input |> Seq.map ((fun row -> (toChoice row.[0], toChoice row.[1])) >> score) |> Seq.reduce (+)
let part2 = input |> Seq.map ((fun row -> (toChoice row.[0], choiceFromResult (toChoice row.[0]) (toResult row.[1]))) >> score) |> Seq.reduce (+)

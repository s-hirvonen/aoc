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
    | _ -> Choice.Scissors

let result (player: Choice) (opponent: Choice): Result = EnumOfValue ((v player - v opponent) %! 3)
let score (opponent, player) =
    match (result player opponent) with
    | Result.PlayerWin -> 6 + v player + 1
    | Result.OpponentWin -> 0 + v player + 1
    | _ -> 3 + v player + 1


let part1 = "2022/inputs/day02.txt" |> System.IO.File.ReadAllLines |> Seq.map (split " " >> (fun ([|a;b|]) -> (toChoice a, toChoice b)) >> score) |> Seq.reduce (+)

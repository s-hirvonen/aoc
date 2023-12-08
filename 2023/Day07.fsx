type Hand = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind

let split (delim: string) (str: string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)
let toPattern = Seq.countBy id >> Seq.map snd >> Seq.sortDescending >> List.ofSeq

let toHand = function
    | [ 5 ]       -> FiveOfAKind  | [ 4; 1 ]    -> FourOfAKind | [ 3; 2 ] -> FullHouse
    | [ 3; 1; 1 ] -> ThreeOfAKind | [ 2; 2; 1 ] -> TwoPair     | [ 2; 1; 1; 1 ] -> OnePair
    | _ -> HighCard

let toJokerHand = function
    | [] -> FiveOfAKind
    | l -> toHand (l.Head + 5 - (List.sum l) :: l.Tail)

let valueOf partNum =
    Seq.map (function
        | 'T' -> 10 | 'J' -> 22 - (11 * partNum)
        | 'Q' -> 12 | 'K' -> 13
        | 'A' -> 14 | n -> int n - int '0')
    >> Seq.fold (fun acc n -> (15 * acc) + n) 0

let solve partNum =
    let patternFn = [ toPattern >> toHand; Seq.filter (fun ch -> ch <> 'J') >> toPattern >> toJokerHand ].[partNum - 1]

    System.IO.File.ReadAllLines "inputs/day07.txt"
    |> Seq.map (split " " >> (fun row -> (patternFn row.[0], valueOf partNum row.[0]), int row.[1]))
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.indexed
    |> Seq.sumBy (fun (rank, bid) -> (rank + 1) * bid)

printfn $"{solve 1}, {solve 2}"

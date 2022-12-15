#r "nuget: FParsec"

open FParsec

[<CustomComparison; CustomEquality>]
type Expr =
    Value of int | List of Expr list

    interface System.IComparable with
        member a.CompareTo b =
            match b with | :? Expr as e -> Expr.compare a e | _ -> invalidArg "b" "fail"

    static member compare a b =
        match (a, b) with
        | (Value av, Value bv) -> Operators.compare av bv
        | (List al , List  bl) -> Operators.compare al bl
        | (List  _, Value _) -> compare a (List [b])
        | (Value _, List  _) -> compare (List [a]) b

    override this.Equals other =
        match other with
        | :? Expr as e -> Expr.compare this e = 0
        | _ -> false

    override this.GetHashCode() = hash this

let split (delim:string) (str:string) = str.Split(delim, System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq
let dividers = [List [List [Value 2]]; List [List [Value 6]]]
let isInList source item = List.contains item source

let pExpr, pExprImpl = createParserForwardedToRef()
let pList = between (pchar '[') (pchar ']') (sepBy pExpr (pchar ',')) |>> List
let pValue = pint32 |>> Value
let validate = function | Success(res, _, _) -> res | Failure _ -> failwith "invalid input"
let check expressions = expressions = List.sort expressions
pExprImpl.Value <- pValue <|> pList

let input =
    "2022/inputs/day13.txt"
    |> System.IO.File.ReadAllText
    |> split "\n\n"
    |> fun packets -> (packets
        |> List.map (split "\n" >> List.map (run pExpr >> validate) >> check)
        |> List.indexed |> List.filter snd |> List.sumBy (fst >> (+) 1)
    , packets
        |> List.map (split "\n") |> List.concat |> List.map (run pExpr >> validate)
        |> List.append dividers |> List.sort |> List.indexed |> List.filter (snd >> isInList dividers)
        |> List.map (fst >> (+) 1) |> List.reduce (*)
    )

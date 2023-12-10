let rec extrapolate tails nums =
    let dx = Seq.windowed 2 >> Seq.map (Seq.rev >> Seq.reduce (-))

    match Seq.forall ((=) 0) nums with
    | true -> Seq.sum tails
    | false -> nums |> dx |> extrapolate ([ Seq.last nums ] @ tails)

let solve part =
    "inputs/day09.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.sumBy (_.Split(" ") >> Seq.map int >> part >> extrapolate [])

printfn $"{solve id}, {solve Seq.rev}"

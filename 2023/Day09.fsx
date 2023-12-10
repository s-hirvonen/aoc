let rec extrapolate tails nums =
    let dx = Seq.windowed 2 >> Seq.map (Seq.rev >> Seq.reduce (-))

    match Seq.forall ((=) 0) nums with
    | true -> tails |> Seq.reduce (+)
    | false -> nums |> dx |> extrapolate (tails |> Seq.append [ Seq.last nums ])

let solve part =
    "inputs/day09.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.sumBy (_.Split(" ") >> Seq.map int >> part >> extrapolate [])

printfn $"{solve id}, {solve Seq.rev}"

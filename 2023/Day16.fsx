type Direction =
    | Up
    | Down
    | Left
    | Right

type Beam = { Direction: Direction; X: int; Y: int }

let moveBeam beam =
    match beam with
    | { Direction = Up } -> { beam with Y = beam.Y - 1 }
    | { Direction = Down } -> { beam with Y = beam.Y + 1 }
    | { Direction = Left } -> { beam with X = beam.X - 1 }
    | { Direction = Right } -> { beam with X = beam.X + 1 }

let turnBeam ch beam =
    match ch, beam with
    | '|', { Direction = Right }
    | '|', { Direction = Left } -> [ { beam with Direction = Up }; { beam with Direction = Down } ]
    | '-', { Direction = Up }
    | '-', { Direction = Down } -> [ { beam with Direction = Left }; { beam with Direction = Right } ]
    | '/', { Direction = Up } -> [ { beam with Direction = Right } ]
    | '/', { Direction = Down } -> [ { beam with Direction = Left } ]
    | '/', { Direction = Left } -> [ { beam with Direction = Down } ]
    | '/', { Direction = Right } -> [ { beam with Direction = Up } ]
    | '\\', { Direction = Up } -> [ { beam with Direction = Left } ]
    | '\\', { Direction = Down } -> [ { beam with Direction = Right } ]
    | '\\', { Direction = Left } -> [ { beam with Direction = Up } ]
    | '\\', { Direction = Right } -> [ { beam with Direction = Down } ]
    | _ -> [ beam ]

let inBounds arr beam =
    beam.X >= 0
    && beam.X < Array2D.length2 arr
    && beam.Y >= 0
    && beam.Y < Array2D.length1 arr

let rec traverse memo (arr: (char * bool)[,]) beams =
    beams
    |> Set.iter (fun beam -> arr[beam.Y, beam.X] <- fst arr[beam.Y, beam.X], true)

    let newBeams =
        beams
        |> Seq.collect (fun beam -> turnBeam (fst arr[beam.Y, beam.X]) beam)
        |> Seq.map moveBeam
        |> Seq.filter (inBounds arr)
        |> set

    if Set.count newBeams = 0 || Set.contains newBeams memo then
        arr
    else
        traverse (memo |> Set.add newBeams) arr newBeams

let input =
    "inputs/day16.txt"
    |> System.IO.File.ReadAllLines
    |> array2D
    |> Array2D.map (fun ch -> ch, false)

let solve =
    Seq.map (fun start ->
        traverse Set.empty<Set<Beam>> (Array2D.copy input) ([ start ] |> set)
        |> Seq.cast<char * bool>
        |> Seq.filter snd
        |> Seq.length)
    >> Seq.max

let part1 = seq { yield { Direction = Right; X = 0; Y = 0 } }

let part2 =
    seq {
        for x in 0 .. Array2D.length2 input - 1 do
            for y in 0 .. Array2D.length1 input - 1 do
                if x = 0 then
                    yield { Direction = Right; X = x; Y = y }

                if y = 0 then
                    yield { Direction = Down; X = x; Y = y }

                if x = Array2D.length2 input - 1 then
                    yield { Direction = Left; X = x; Y = y }

                if y = Array2D.length1 input - 1 then
                    yield { Direction = Up; X = x; Y = y }
    }

printfn $"{solve part1}, {solve part2}"

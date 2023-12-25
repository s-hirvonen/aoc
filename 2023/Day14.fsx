let moveNorth (arr: char[,]) =
    for j in 0 .. (Array2D.length2 arr - 1) do
        let mutable stop = -1
        let mutable moved = 0

        for i in 0 .. (Array2D.length1 arr - 1) do
            match arr[i, j] with
            | '#' ->
                stop <- i
                moved <- 0
            | 'O' ->
                arr[i, j] <- '.'
                arr[stop + moved + 1, j] <- 'O'
                moved <- moved + 1
            | _ -> ()

    arr

let moveSouth (arr: char[,]) =
    for j in 0 .. (Array2D.length2 arr - 1) do
        let mutable stop = Array2D.length2 arr
        let mutable moved = 0

        for i = (Array2D.length1 arr - 1) downto 0 do
            match arr[i, j] with
            | '#' ->
                stop <- i
                moved <- 0
            | 'O' ->
                arr[i, j] <- '.'
                arr[stop - moved - 1, j] <- 'O'
                moved <- moved + 1
            | _ -> ()

    arr


let moveWest (arr: char[,]) =
    for i in 0 .. (Array2D.length1 arr - 1) do
        let mutable stop = -1
        let mutable moved = 0

        for j in 0 .. (Array2D.length2 arr - 1) do
            match arr[i, j] with
            | '#' ->
                stop <- j
                moved <- 0
            | 'O' ->
                arr[i, j] <- '.'
                arr[i, stop + moved + 1] <- 'O'
                moved <- moved + 1
            | _ -> ()

    arr

let moveEast (arr: char[,]) =
    for i in 0 .. (Array2D.length1 arr - 1) do
        let mutable stop = Array2D.length1 arr
        let mutable moved = 0

        for j = (Array2D.length2 arr - 1) downto 0 do
            match arr[i, j] with
            | '#' ->
                stop <- j
                moved <- 0
            | 'O' ->
                arr[i, j] <- '.'
                arr[i, stop - moved - 1] <- 'O'
                moved <- moved + 1
            | _ -> ()

    arr

let cycle = moveNorth >> moveWest >> moveSouth >> moveEast

let toString = Seq.cast<char> >> System.String.Concat

let repeat n arr =
    let memo = System.Collections.Generic.Dictionary<string, int>()

    let rec doit n (arr: char[,]) =
        let key = toString arr

        if n <= 0 then
            arr
        elif memo.ContainsKey(key) then
            let cycleSize = memo.Item(key) - n

            if n - cycleSize > 0 then
                doit (n % cycleSize) (arr)
            else
                doit (n - 1) (cycle arr)
        else
            memo.Add(key, n)
            doit (n - 1) (cycle arr)

    doit n arr

let answer arr = (fun i _ ch -> if ch = 'O' then (Array2D.length1 arr - i) else 0)

let solve op =
    "inputs/day14.txt"
    |> System.IO.File.ReadAllLines
    |> array2D
    |> op
    |> fun arr -> Array2D.mapi (answer arr) arr
    |> Seq.cast<int>
    |> Seq.sum

printfn $"{solve moveNorth} {solve (repeat 1000000000)}"

let input = System.IO.File.ReadAllLines "2022/inputs/day08.txt" |> Seq.map (Seq.map (fun ch -> int ch - int '0')) |> array2D

let isVisible row i value =
    let left = (Seq.take i row)
    let right = Seq.skip (i + 1) row
    (Seq.forall ((>)value) left) || (Seq.forall ((>)value) right)

let countVisible row i value =
    let visible = Seq.takeWhile ((>)value) >> Seq.length >> (+)1
    let left  = row |> Seq.take i |> Seq.rev |> visible |> min i
    let right = row |> Seq.skip (i + 1)      |> visible |> min ((Seq.length row) - i - 1)
    left * right

let solve operation combinator = 
    Array2D.mapi (fun y x i -> combinator (operation input.[*,x] y i) (operation input.[y,*] x i))

input |> (solve isVisible   (||)) |> (Seq.cast<bool> >> Seq.filter id >> Seq.length), // part 1
input |> (solve countVisible (*)) |> (Seq.cast<int>  >> Seq.max)                      // part 2

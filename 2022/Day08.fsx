let input = System.IO.File.ReadAllLines "2022/inputs/day08.txt" |> Seq.map (Seq.map System.Char.ToString) |> array2D

let isVisible row i value = [Seq.take i row; Seq.skip (i + 1) row] |> List.map (Seq.forall ((>)value))

let countVisible row i value =
    let visible = Seq.takeWhile ((>)value) >> Seq.length >> (+)1
    [row |> Seq.take i |> Seq.rev |> visible |> min i;
     row |> Seq.skip (i + 1)      |> visible |> min ((Seq.length row) - i - 1)]

let solve operation combinator = 
    Array2D.mapi (fun y x i -> operation input.[*,x] y i @ operation input.[y,*] x i |> Seq.reduce combinator)

input |> solve isVisible   (||) |> (Seq.cast<bool> >> Seq.filter id >> Seq.length), // part 1
input |> solve countVisible (*) |> (Seq.cast<int>  >> Seq.max)                      // part 2

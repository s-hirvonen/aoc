let inline (++) x = x + 1

let neighbors r c (A:'a[,]) =
    [if r > 0 then yield A.[r-1,c]
     if r < Array2D.length1 A - 1 then yield A.[r+1,c]
     if c > 0 then yield A.[r,c-1]
     if c < Array2D.length2 A - 1 then yield A.[r,c+1]]

let localBottom arr (r, c) = (neighbors r c arr) |> List.forall (fun node -> node > (Array2D.get arr r c))

let input = 
    "2021/inputs/day09.txt" 
    |> System.IO.File.ReadAllLines 
    |> Seq.map (Seq.map (fun ch -> int ch - int '0'))
    |> array2D

input |> Array2D.mapi (fun x y n -> (n, localBottom input (x, y))) |> Seq.cast<(int * bool)> |> Seq.filter snd |> Seq.sumBy (fst >> (++))

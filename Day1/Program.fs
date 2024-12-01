let array1, array2 = 
    System.IO.File.ReadAllLines "input"
    |> Seq.map (fun line -> line.Split(' ') |> Seq.filter (fun i -> not(System.String.IsNullOrWhiteSpace(i))) |> Seq.map int |> Seq.toArray)
    |> Seq.map (fun line -> (line.[0], line.[1]))
    |> Seq.toArray
    |> Array.unzip

let ans = 
    Array.zip (Array.sort array1) (Array.sort array2)
    |> Seq.map (fun (a, b) -> System.Math.Abs(a - b))
    |> Seq.sum

printfn $"The anser is: {ans}"


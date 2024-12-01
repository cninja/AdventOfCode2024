let example = "3   4
4   3
2   5
1   3
3   9
3   3"

let array1, array2 = 
    System.IO.File.ReadAllLines "input"
    //example.Split '\n'
    |> Seq.map (fun line -> line.Split(' ') |> Seq.filter (fun i -> not(System.String.IsNullOrWhiteSpace(i))) |> Seq.map int |> Seq.toArray)
    |> Seq.map (fun line -> (line.[0], line.[1]))
    |> Seq.toArray
    |> Array.unzip

let ans = 
    array1
    |> Array.map (fun i -> array2 |> Array.map (fun j -> if i = j then i else 0) |> Seq.sum)
    |> Seq.sum

printfn $"The anser is: {ans}"


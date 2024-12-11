
let inputA = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

let inputFile = System.IO.File.ReadAllText "input"

let map =
    // inputA.Split "\n"
    inputFile.Split "\n"
    |> Seq.map (fun line -> line |> Seq.toArray |> Seq.map (fun c -> try int(c.ToString()) with _ -> -1))
    |> Seq.toArray
    |> array2D

let directions = [(1, 0); (0, 1); (-1, 0); (0, -1)]

let rec pathCount i j curVal: (int * int) list =
    let c = try map[i,j] with _ -> -1
    if not (c = curVal) then
        []
    else if curVal = 9 then
        [(i, j)]
    else
        let v =
            directions
            |> Seq.collect (fun (di, dj) -> pathCount (i + di) (j + dj) (curVal + 1))
            |> Seq.distinct
            |> Seq.toList
        // if curVal = 0 then
        //     printfn $"PathCount {i} {j} {curVal}: {v}"
        v
        
let ans =
    {0.. Array2D.length1 map}
    |> Seq.collect (fun i -> {0..Array2D.length2 map} |> Seq.map (fun j -> (i, j)))
    |> Seq.map (fun (i,j) -> (pathCount i j 0).Length)
    |> Seq.sum

printfn $"Ans: {ans}"

let rec pathCount2 i j curVal: int =
    let c = try map[i,j] with _ -> -1
    if not (c = curVal) then
        0
    else if curVal = 9 then
        1
    else
        let v =
            directions
            |> Seq.map (fun (di, dj) -> pathCount2 (i + di) (j + dj) (curVal + 1))
            |> Seq.sum
        // if curVal = 0 then
        //     printfn $"PathCount {i} {j} {curVal}: {v}"
        v
let ans2 =
    {0.. Array2D.length1 map}
    |> Seq.collect (fun i -> {0..Array2D.length2 map} |> Seq.map (fun j -> (i, j)))
    |> Seq.map (fun (i,j) -> pathCount2 i j 0)
    |> Seq.sum

printfn $"Ans2: {ans2}"

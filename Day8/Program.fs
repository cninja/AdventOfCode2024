
let inputA = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

let inputFile = System.IO.File.ReadAllText "input"

let cleanMaze =
    //inputA.Split "\r\n"
    inputFile.Split "\r\n"
    |> Seq.map Seq.toArray
    |> Seq.toArray
    |> array2D
let mutable maze = Array2D.copy cleanMaze
let mutable nodeList = Array2D.copy cleanMaze


let getMazeChar (i, j) (myMaze: char array2d) =
    //try myMaze.[i,j] with _ -> '.'
    myMaze.[i,j]
    
let markNode i j c =
    printfn $"{i},{j}"
    if not (c = '.') then
        Array2D.iteri (fun i2 j2 c2 ->
            if c2 = c && not ((i, j) = (i2, j2)) then
                let di = i - i2
                let dj = j - j2
                try nodeList.[i + di, j + dj] <- '*' with _ -> ()
        ) maze


Array2D.iteri markNode maze
let n = nodeList

let ans = 
    nodeList
    |> Seq.cast<char>
    |> Seq.where (fun a -> a = '*')
    |> Seq.length

printfn $"Ans: {ans}"

let markNode2 i j c =
    printfn $"{i},{j}"
    if not (c = '.') then
        Array2D.iteri (fun i2 j2 c2 ->
            if c2 = c && not ((i, j) = (i2, j2)) then
                let mutable k = 0
                let di = i - i2
                let dj = j - j2
                while -1 < (i + di * k) && (i + di * k) < (Array2D.length1 maze) do
                    try nodeList.[i + (di * k), j + (dj * k)] <- '*' with _ -> ()
                    k <- k + 1
        ) maze

Array2D.iteri markNode2 maze
let n2 = nodeList

let ans2 = 
    nodeList
    |> Seq.cast<char>
    |> Seq.where (fun a -> a = '*')
    |> Seq.length

printfn $"Ans2: {ans2}"

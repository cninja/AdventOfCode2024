
let input = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

let inputFile = System.IO.File.ReadAllText "input"

let cleanMaze =
    // input.Split "\n"
    inputFile.Split "\n"
    |> Seq.map Seq.toArray
    |> Seq.toArray
    |> array2D
let mutable maze = Array2D.copy cleanMaze

let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]
let mutable directionIndex = 0

let getMazeChar (i, j) (myMaze: char array2d) =
    try myMaze.[i,j] with _ -> ' '
    
let mutable currentPos =
    { 0..Array2D.length1 maze}
    |> Seq.collect (fun i -> {0..Array2D.length2 maze} |> Seq.map (fun j -> (i, j)))
    |> Seq.tryFind (fun (i, j) -> getMazeChar (i, j) maze = '^')
    |> Option.defaultValue (0, 0)

while not ((getMazeChar currentPos maze) = ' ') do
    let (i, j) = currentPos
    let (di, dj) = directions.[directionIndex]
    let nextPos = (i + di, j + dj)
    maze.[i,j] <- 'X'
    if getMazeChar nextPos maze = '#' then
        directionIndex <- (directionIndex + 1) % directions.Length
    else
        currentPos <- nextPos

let mutable ans = 0
Array2D.iter (fun c -> if c = 'X' then ans <- ans + 1) maze
    
printfn $"Ans: {ans}"

let isLoopingMaze (myMaze: char array2d): bool =
    let mutable currentPos =
        { 0..Array2D.length1 myMaze}
        |> Seq.collect (fun i -> {0..Array2D.length2 myMaze} |> Seq.map (fun j -> (i, j)))
        |> Seq.tryFind (fun (i, j) -> getMazeChar (i, j) myMaze = '^')
        |> Option.defaultValue (0, 0)
    let mutable steps = cleanMaze.Length
    directionIndex <- 0

    while not ((getMazeChar currentPos myMaze) = ' ') && steps > 0 do
        steps <- steps - 1
        let (i, j) = currentPos
        let (di, dj) = directions.[directionIndex]
        let nextPos = (i + di, j + dj)
        myMaze.[i,j] <- 'X'
        if getMazeChar nextPos myMaze = '#' then
            directionIndex <- (directionIndex + 1) % directions.Length
        else
            currentPos <- nextPos
        
    steps = 0

let mutable ans2 = 0
Array2D.iteri (fun i j _ ->
    let myMaze = Array2D.copy cleanMaze
    myMaze.[i,j] <- '#'
    if isLoopingMaze myMaze then
        ans2 <- ans2 + 1
    ) cleanMaze

printfn $"Ans2: {ans2}"

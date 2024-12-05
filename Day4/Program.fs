open System.Linq

let input = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
let inputFile = System.IO.File.ReadAllText "input"

let chars =
    inputFile.Split "\n"
    |> Seq.map Seq.toList
    |> Seq.toList
    
let pattern = "XMAS" |> Seq.toList
let directions = [(1,1); (1,0); (1,-1); (0,1); (0,-1); (-1,1); (-1,0); (-1,-1)]

let xmasCount (i: int) (j: int): int =
    let rec isXmas i2 j2 (currentPattern: char list) direction =
        let (di, dj) = direction
        let currentChar = try chars.[i2].[j2] with _ -> ' '
        match currentPattern with
        | [] -> true
        | expectedChar::tail when expectedChar = currentChar -> isXmas (i2 + di) (j2 + dj) tail direction
        | _ -> false
        
    directions
        |> Seq.where (fun direction -> isXmas i j pattern direction)
        |> Seq.length
let found =
    Enumerable.Range(0, chars.Length)
    |> Seq.map (fun i -> Enumerable.Range(0, chars.[i].Length) |> Seq.map (fun j -> xmasCount i j) |> Seq.sum)
    |> Seq.sum
    

printfn $"Ans: {found}"

//Part 2
let isMass i j =
    let validDiag (di, dj) =
        let d1 = try chars.[i + di].[j + dj] with _ -> ' '
        let d2 = try chars.[i - di].[j - dj] with _ -> ' '
        (d1 = 'M' && d2 = 'S') || (d1 = 'S' && d2 = 'M')
        
    let currentChar = try chars.[i].[j] with _ -> ' '
    currentChar = 'A' && validDiag (1, 1) && validDiag (1, -1)
let found2 =
    Enumerable.Range(0, chars.Length)
    |> Seq.map (fun i -> Enumerable.Range(0, chars.[i].Length) |> Seq.where (fun j -> isMass i j) |> Seq.length)
    |> Seq.sum
    
printfn $"Ans2: {found2}"

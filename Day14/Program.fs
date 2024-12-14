
open System
open System.Collections
open System.Text.RegularExpressions

let input = "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

let inputFile = System.IO.File.ReadAllText "input"

type Robot = ((int * int) * (int * int))

// let roomX = 11
// let roomY = 7
let roomX = 101
let roomY = 103
let robots =
    Regex.Matches(inputFile, @"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
    |> Seq.map (fun m -> ((int m.Groups.[1].Value, int m.Groups.[2].Value),(int m.Groups.[3].Value, int m.Groups.[4].Value)))
    |> Seq.toArray

let modu x n = ((x % n) + n) % n

let addSeconds (r:Robot) (s: int):(int*int) =
    let ((x,y),(dx,dy)) = r
    let newX = x + (dx * s)
    let newY = y + (dy * s)
    (modu newX roomX, modu newY roomY)
    
let exampleRobot = ((2,4),(2,-3))
let setps =
    {0..5}
    |> Seq.map (addSeconds exampleRobot)
    |> Seq.toArray
    
let finalPosition =
    robots
    |> Seq.map (fun r -> addSeconds r 100)
    |> Seq.toArray


let quadLimitX = roomX / 2 
let quadLimitY = roomY / 2

let quadCount fn1 fn2 =
    finalPosition
    |> Seq.where (fun p ->
        let (x, y) = p
        fn1 x quadLimitX && fn2 y quadLimitY)
    |> Seq.length
let q1 = quadCount (<) (<)
let q2 = quadCount (<) (>)
let q3 = quadCount (>) (<)
let q4 = quadCount (>) (>)
let ans = q1 * q2 * q3 * q4
printfn $"Ans: {ans}"

let printPositionsAt s =
    let positionLookup = 
        robots
        |> Seq.map (fun r ->
            let (x, y) = addSeconds r s
            x * 1000 + y)
        |> Set.ofSeq
        
    
    let room =
        {0..roomY-1}
        |> Seq.map (fun y ->
            {0..roomX-1}
            |> Seq.map (fun x ->
                if positionLookup.Contains (x * 1000 + y) then '1' else '.')
            |> String.Concat)
        |> (String.concat "\n")
    
    let streaks = Regex.Matches(room, @"11111").Count
    if streaks > 5 then
        printfn $"Time: {s}\n{room}\n\n"
    else if s % 1000 = 0 then
        printfn $"Tick: {s}"
    
{0..100000}
    |> Seq.map printPositionsAt
    |> Seq.toArray
    |> ignore
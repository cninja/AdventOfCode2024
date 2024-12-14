
open System
open System.Text.RegularExpressions

let input = "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

let inputFile = System.IO.File.ReadAllText "input"

type Machine =
    struct
        val buttonA: (decimal*decimal)
        val buttonB: (decimal*decimal)
        val prize: (decimal*decimal)
        new (ax, ay, bx, by, px, py) =           // Constructor definition
           {buttonA = (decimal ax, decimal ay)
            buttonB = (decimal bx, decimal by)
            prize = (decimal px, decimal py);} 
    end
type Play = (decimal*decimal)

let playMachine (m:Machine): Play option =
    let (ax, ay) = m.buttonA
    let (bx, by) = m.buttonB
    let (px_part, py_part) = m.prize
    //Part2
    let px = px_part + 10000000000000.0m
    let py = py_part + 10000000000000.0m
    
    let s1 = py / ay
    let i = ((py / ay) - (by * px) / (ay * bx)) / ((decimal 1) - (by * ax) / (ay * bx))
    let j = (px - i*ax)/bx
    
    let ri = Math.Round i
    let rj = Math.Round j
    if Math.Abs(i - ri) < 0.00001m && Math.Abs(j - rj) < 0.00001m then
        Some (ri, rj)
    else
        None
    
    
let costForPlay (p:Play) =
    let(a,b) = p
    a * 3m + b
    
    
let machines =
    Regex.Matches(inputFile, @"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")
    |> Seq.map (fun m -> Machine(m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value, m.Groups.[5].Value, m.Groups.[6].Value))
    |> Seq.choose playMachine
    |> Seq.toArray

let ans =
    machines
    // |> Seq.length
    |> Seq.map costForPlay
    |> Seq.sum
    
    
printfn $"Ans: {ans}"

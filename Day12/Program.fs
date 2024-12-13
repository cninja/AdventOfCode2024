
open System.Collections.Generic
open System

let input = "AAAA
BBCD
BBCC
EEEC"

let input2 = "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"

let input3 = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

let input4 = "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"

let input5 = "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"
let inputFile = System.IO.File.ReadAllText "input"

let map =
    // input5.Split "\n"
    inputFile.Split "\n"
    |> Seq.map Seq.toArray
    |> Seq.toArray
    |> array2D

type Garden = (char * List<int * int>)

let isAjacent (i: int) (j: int) plot =
    let (pi, pj) = plot
    let di = Math.Abs(i - pi)
    let dj = Math.Abs(j - pj)
    di + dj = 1
    
let isGardenAjacent c i j (g: Garden): bool =
    let (gChar, plots) =  g
    gChar = c && Seq.exists (isAjacent i j) plots

let combineGardens (gs:Garden list): Garden  =
    let (c,_) = gs.Head
    let allPlots = gs |> Seq.collect snd |> Seq.toArray
    (c, List<int*int>(allPlots))
    
let directions = [(0,1);(0,-1);(1,0);(-1,0)]

let calcPlotPrice (plots:List<int*int>) (plot:int*int):bigint =
   let (i,j) = plot
   directions
    |> Seq.where (fun (di,dj) -> not(plots.Contains((i + di, j + dj))))
    |> Seq.length
    |> bigint

let calcPrice (g:Garden): bigint =
    let(_,plots) = g
    let fenceSize =
        plots
        |> Seq.map (calcPlotPrice plots)
        |> Seq.sum
    fenceSize * bigint plots.Count
    
let gardens = List<Garden>()

map
    |> Array2D.iteri (fun i j c ->
        if i = 2 && j = 6 then
            printfn "h"
        let gardenMatches = Seq.where (isGardenAjacent c i j) gardens |> Seq.toList
        match gardenMatches with
        | [] -> gardens.Add((c, List<int*int>[(i,j)]))
        | [garden] ->
            let (_, plots) = garden
            plots.Add((i,j))
        | _ ->
            gardenMatches |> Seq.iter (fun g -> gardens.Remove g |> ignore)
            let combinedG = combineGardens gardenMatches
            let (_, plots) = combinedG
            plots.Add((i,j))
            gardens.Add(combinedG)
        )

let ans = gardens |> Seq.map calcPrice |> Seq.sum
printfn $"Ans: {ans}"

let toInt = function true -> 1 | false -> 0

let calcPlotPrice2 (plots:List<int*int>) (plot:int*int):bigint =
   let (i,j) = plot
   let at di dj = plots.Contains((i + di, j + dj))
   let notAt di dj = not(at di dj)
   let cornerNW = toInt (
   //  X
   // XA
       (notAt -1 0) && (notAt 0 -1))
   let cornerNE = toInt (
   // X 
   // AX
       (notAt -1 0) && (notAt 0 1))
   let cornerSW = toInt (
   // XA
   //  X
        ((notAt 1 0) && (notAt 0 -1))
   // XA
   // aa
        || ((at 1 0) && (at 1 -1) && (notAt 0 -1)))
   let cornerSE = toInt (
   // AX
   // X
        ((notAt 1 0) && (notAt 0 1))
   // AX
   // aa
        || ((at 1 0) && (at 1 1) && (notAt 0 1))
   // Aa
   // Xa
        || ((notAt 1 0) && (at 1 1) && (at 0 1))
   // Aa
   // aX
        || ((at 1 0) && (notAt 1 1) && (at 0 1)))
                        
   bigint (cornerNW + cornerNE + cornerSW + cornerSE)

let calcPrice2 (g:Garden): bigint =
    let(_,plots) = g
    let fenceSize =
        plots
        |> Seq.map (calcPlotPrice2 plots)
        |> Seq.sum
    fenceSize * bigint plots.Count
    
let ans2 = gardens |> Seq.map calcPrice2 |> Seq.sum
printfn $"Ans2: {ans2}"

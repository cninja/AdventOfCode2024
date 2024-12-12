
open System.Collections.Generic
open System.Numerics

// let input = "125 17"
let input = "4 4841539 66 5279 49207 134 609568 0"

let stones =
    input.Split " "
    |> Seq.map (fun s -> bigint.Parse(s))
            
let blinkRule (n: bigint): bigint seq =
    let digits = n.ToString()
    let dLength = digits.Length
    match n with
    | _ when n.IsZero -> [1I]
    | _ when dLength % 2 = 0 ->
        [
         bigint.Parse(digits.Substring(0, dLength / 2))
         bigint.Parse(digits.Substring(dLength / 2))
         ]
    | _ -> [n * 2024I]
    
let blink s =
    s
    |> Seq.collect blinkRule

let repeat n fn =
    Seq.init n (fun _ u -> fn u)
    |> Seq.reduce (>>)
let ans =
    stones
    |> repeat 25 blink
    |> Seq.length
    
printfn $"Ans: {ans}"

// let stones2 = List<_>(stones |> Seq.map (fun i -> (int i, i.ToString())))
//             
// let memoIntRule = Dictionary<int, List<(int * string)> -> unit>()
// let memoStringRule = Dictionary<string, List<(int * string)> -> unit>()
//
// let buildStone (bi:BigInteger) =
//     let i = try int bi with _ -> -1
//     (i, bi.ToString())
//     
// let blinkRule2 (d: string): List<(int * string)> -> unit =
//     let dLength = d.Length
//     match d with
//     | "0" ->
//         let s1 = buildStone 1I
//         (fun (s:List<(int * string)>) -> s.Add(s1))
//     | _ when dLength % 2 = 0 ->
//          let i1 = bigint.Parse(d.Substring(0, dLength / 2))
//          let s1 = buildStone i1
//          let i2 = bigint.Parse(d.Substring(dLength / 2))
//          let s2 = buildStone i2
//          (fun (s:List<(int * string)>) ->
//              s.Add(s1)
//              s.Add(s2))
//     | _ ->
//         let i1 = bigint.Parse(d) * 2024I
//         let s1 = buildStone i1
//         (fun (s:List<(int * string)>) -> s.Add(s1))
//         
// let blink2 (myStones: List<(int * string)>) =
//     let ansStones = List<(int * string)>(myStones.Count * 2)
//     let mutable fn = Unchecked.defaultof<List<(int * BigInteger)> -> unit>
//     for (i,s) in myStones do
//         if i > -1 then
//             match memoIntRule.TryGetValue i with
//             | true, fn -> fn ansStones
//             | _ ->
//                 let fn = blinkRule2 s
//                 memoIntRule.Add(i, fn)
//                 fn ansStones
//         else
//             match memoStringRule.TryGetValue s with
//             | true, fn -> fn ansStones
//             | _ ->
//                 let fn = blinkRule2 s
//                 memoStringRule.Add(s, fn)
//                 fn ansStones
//     ansStones
//         
//
// let rec repeat2 (n:int) fn (s: List<(int * string)>) =
//     if n = 0 then
//         s
//     else
//         let s2 = fn s
//         printfn $"On {n} {s.Count}"
//         repeat2 (n - 1) fn s2
// let ans2 =
//     repeat2 75 blink2 stones2
//     |> Seq.length
//     
let stoneCountCache = Dictionary<BigInteger * int, BigInteger>()
let rec stoneCount (s: BigInteger * int): BigInteger =
    let (num, steps) = s
    if steps = 0 then
        1
    else
        match stoneCountCache.TryGetValue s with
        | true, count -> count
        | _ ->
            let (num, steps) = s
            let newSteps = steps - 1
            let count =
                blinkRule num
                |> Seq.map (fun i -> stoneCount (i, newSteps))
                |> Seq.sum
            stoneCountCache.Add(s, count)
            count
    
let ans2 =
    stones
    |> Seq.map (fun i -> (i, 75))
    |> Seq.map stoneCount
    |> Seq.sum

printfn $"Ans2: {ans2}"

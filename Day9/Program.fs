
open System.Linq

let input = "2333133121414131402"
let inputFile = System.IO.File.ReadAllText "input"

let diskSteps =
    // input
    inputFile
    |> Seq.toArray
    |> Seq.map (fun c -> c.ToString())
    |> Seq.map int
    |> Seq.toList
 
 
let rec makeDiskStep (diskHead: int list) (steps: int list) (isEmpty: bool) i =
    let digit = if isEmpty then -1 else i
    let nextI = if isEmpty then i else i + 1
    match steps with
    | [] -> diskHead
    | h::tail ->
        let diskPart = List.replicate h digit
        makeDiskStep (diskHead @ diskPart) tail (not isEmpty) nextI
let mutable disk = makeDiskStep [] diskSteps false 0 |> Seq.toArray
let rec defrag i j =
    let c1 = disk[i]
    let c2 = disk[j]
    match c1 with
    | _ when i > j -> disk
    | -1 when c2 = -1 ->
        defrag i (j - 1)
    | -1 ->
        disk[i] <- c2
        disk[j] <- c1
        defrag (i + 1) j
    | _ -> defrag (i + 1) j

defrag 0 (disk.Length - 1)

let ans =
    {0..disk.Length - 1}
    |> Seq.map (fun i ->
        let c = if disk[i] = -1 then 0 else disk[i]
        bigint (i * c))
    |> Seq.sum
printfn $"Ans: {ans}"

let mutable disk2 = makeDiskStep [] diskSteps false 0 |> Seq.toArray

let rec trySwap i len j len2 =
    if i < j then
        false
    else if len = len2 then
        {0..len - 1}
            |> Seq.map (fun x ->
                let c = disk2[i + x]
                disk2[i + x] <- disk2[j - x - 1]
                disk2[j - x - 1] <- c)
            |> Seq.toArray
            |> ignore
        true
    else
        let isOpen = disk2[j] = -1
        let nextLen = if isOpen then (len2 + 1) else 0
        trySwap i len (j + 1) nextLen
        
    
let rec defrag2 i curLength curSwap =
    let c = disk2[i]
    let c1 = disk2[i - 1]
    if i = 1 then
        disk2
    else if c1 = c then
       defrag2 (i - 1) (curLength + 1) curSwap
    else if c = curSwap then
        printfn $"Swapping: {curSwap} on {c}"
        trySwap i curLength 0 0 |> ignore
        defrag2 (i - 1) 1 (curSwap - 1)
    else
        defrag2 (i - 1) 1 curSwap

defrag2 (disk2.Length - 1) 1 (disk2.Last()) |> ignore

//6332189866718
//6353648838485
let ans2 =
    {0..disk2.Length - 1}
    |> Seq.map (fun i ->
        let c = if disk2[i] = -1 then 0 else disk2[i]
        bigint (i * c))
    |> Seq.sum
printfn $"Ans2: {ans2}"

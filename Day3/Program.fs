open System.Text.RegularExpressions

//let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
//let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let input = System.IO.File.ReadAllText "input"

let extractDos (input: string): string =
    match input.Split "do()" |> Seq.toList with
    | h::t -> String.concat " " t
    | [] -> ""

let first::tail = input.Split "don't()" |> Seq.toList
let input2 = String.concat " " (first :: (tail |> Seq.map extractDos |> Seq.toList))

let ans = 
    Regex.Matches(input2, @"mul\((\d+),(\d+)\)")
    |> Seq.map (fun m -> (int m.Groups.[1].Value) * (int m.Groups.[2].Value))
    |> Seq.sum

printfn $"Ans: {ans}"
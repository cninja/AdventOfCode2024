
let input = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"
let inputFile = System.IO.File.ReadAllText "input"
let arrayToTuple (a: 'a array) = (a.[0], a.[1])

let (rulesRaw, books) =
    inputFile.Split("\n\n")
    |> Seq.map (fun lines -> lines.Split("\n") |> Seq.map (fun line -> line.Split [|'|'; ','|]))
    |> Seq.toArray
    |> arrayToTuple

let violationRules =
    rulesRaw
    |> Seq.map (Array.rev >> arrayToTuple)
    
let rec isBookValid (book: string list): bool =
    match book with
    | p1::p2::tail -> not (Seq.contains (p1, p2) violationRules) && isBookValid (p2::tail)
    | [ p1 ] -> true
    | _ -> false

let validBooks =
    books
    |> Seq.map Seq.toList
    |> Seq.where isBookValid
    
let ans =
    validBooks
    |> Seq.map (fun book -> int book.[(book.Length - 1) / 2])
    |> Seq.sum
    
printfn $"Ans: {ans}"

let invalidBooks =
    books
    |> Seq.map Seq.toList
    |> Seq.where (isBookValid >> not)

let rec correct headBook tailBook: string list =
    match tailBook with
    //Swap pages and redo the entire correction
    | [] -> headBook
    | p1::p2::tail when Seq.contains (p1, p2) violationRules -> correct [] (headBook @ [p2] @ [p1] @ tail)
    | p1::tail -> correct (headBook @ [p1]) tail
        
let corrected =
    invalidBooks
    |> Seq.map (fun book -> correct [] book)
let ans2 =
    invalidBooks
    |> Seq.map (fun book -> correct [] book)
    |> Seq.map (fun book -> int book.[(book.Length - 1) / 2])
    |> Seq.sum
    
printfn $"Ans2: {ans2}"

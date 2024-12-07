

type Equation = {Total: bigint; Numbers: bigint list}

let input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

let inputFile = System.IO.File.ReadAllText "input"

let eqs = 
    //input.Split "\n"
    inputFile.Split "\n"
    |> Seq.map (fun line ->
        let parts = line.Split ":"
        let total = bigint.Parse parts[0]
        let numbers = 
            parts[1].Split " "
            |> Seq.where (System.String.IsNullOrWhiteSpace >> not)
            |> Seq.map bigint.Parse
            |> Seq.toList

        { Total = total; Numbers = numbers}
    )

let concat x y = bigint.Parse (string x + string y)

let rec isValidEquation goal currentTotal remaining =
    match remaining with
    | [] -> goal = currentTotal
    | h::tail -> 
        (isValidEquation goal (currentTotal + h) tail) 
        || (isValidEquation goal (currentTotal * h) tail)
        || (isValidEquation goal (concat currentTotal h) tail)


let isValidEquation2 (e: Equation): bool =
    match e.Numbers with
    | h1::h2::tail -> isValidEquation e.Total h1 (h2::tail) || isValidEquation e.Total (concat h1 h2) tail
    | [h] -> h = e.Total
    | _ -> false

    

let ans = 
    eqs
    |> Seq.where isValidEquation2
    |> Seq.map (fun a -> a.Total)
    |> Seq.sum
printfn $"Ans: {ans}" 
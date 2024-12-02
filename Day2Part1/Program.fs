let levels = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

let rec difference (report: int list) : int list = 
    match report with
    | e1 :: e2 :: tail -> (e1 - e2) :: difference (e2 :: tail)
    | _ -> []

let isDiffer (report: int list) : bool = 
    match report with
    | e1 :: e2 :: _ -> (System.Math.Abs (e1 - e2)) > 0 
    | _ -> true

let isValidReport (report: int list) : bool =
    Seq.forall (fun i -> 0 < i && i < 4) report || Seq.forall (fun i -> -4 < i && i < 0) report
let ans = 
    System.IO.File.ReadAllLines "input"
    //levels.Split "\n"
    |> Seq.map (fun l -> l.Split " " |> Seq.map int)
    |> Seq.map Seq.toList
    |> Seq.map difference
    |> Seq.where isValidReport

printfn $"Ans: {Seq.length ans}"

let removeEachElement report =
  let rec removeAt index list =
    match index, list with
    | 0, _ :: tail -> tail
    | i, head :: tail -> head :: removeAt (i - 1) tail
    | _, [] -> []

  [0 .. List.length report - 1]
  |> List.map (fun i -> removeAt i report)



let ans2 = 
    System.IO.File.ReadAllLines "input"
    //levels.Split "\n"
    |> Seq.map (fun l -> l.Split " " |> Seq.map int)
    |> Seq.map Seq.toList
    |> Seq.map (fun report -> report :: removeEachElement report)
    |> Seq.map (fun reportOptions -> Seq.map difference reportOptions)
    |> Seq.where (fun reportOptions -> Seq.exists isValidReport reportOptions)

printfn $"Ans2: {Seq.length ans2}"

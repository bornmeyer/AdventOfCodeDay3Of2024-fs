module Solver
open System.IO

let rec find' (line:char array) (whatToFind:char array) (pos:int) acc =     
    let (newPos, newAcc) = 
        if line[pos .. (pos + ((Array.length whatToFind) - 1))] = whatToFind then
            ((pos + ((Array.length whatToFind) - 1)),  ([(pos, (pos + (Array.length whatToFind) - 1))] @ acc))
        else
            ((pos + 1), acc)
    if pos = ((line |> Array.length) - (whatToFind |> Array.length)) then
        acc |> List.rev
    else
        find' line whatToFind newPos newAcc


let find (whatToFind:string) line  = 
    match line with    
    | "" -> []
    | _ -> find' (line.ToCharArray()) (whatToFind.ToCharArray()) 0 []
       
let processValues (valueString:string) = 
    match valueString.Split(",") with
    | [| left; right |] -> Some (left |> int, right |> int, valueString)
    | [|_|] -> None
    | [||] -> None
    | _ -> None
   
let isValid char = char |> System.Char.IsNumber || char = ','

let rec extractValues (line:char array) acc listOfPositions =
    match listOfPositions with
    | (posStart, posEnd) :: tail when line[posEnd + 1] = '(' ->         
        let v = (line[(posEnd + 2) ..] |> Array.takeWhile(fun x -> x <> ')' && x |> isValid)) |> System.String |> processValues
        match v with
        | Some x -> extractValues line ([x] @ acc) tail
        | None -> extractValues line acc tail
    | _ :: tail -> extractValues line acc tail
    | [] -> acc


let solveSingle (line:string) =
    let lineAsArray = line.ToCharArray()
    line 
    |> find "mul" 
    |> extractValues lineAsArray []
    |> Seq.map(fun (x, y, _) -> x*y)
    |> Seq.sum
    //|> List.sumBy(fun (x, y, _) -> x * y )

let solve path = 
    File.ReadLines(path)
    |> Seq.map(solveSingle)
    |> Seq.sum
    //|> Seq.collect(solveSingle)
    //|> Seq.sumBy(solveSingle)
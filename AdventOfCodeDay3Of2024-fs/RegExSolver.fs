module RegExSolver

open System.Text.RegularExpressions
open System.IO
open System

let mappedValues (match':Match) transformer =  [ for g in match'.Groups -> transformer(g) ]

let filterValid groupValues canExecute acc = 
        match groupValues with
        | ["do()"; _; _] -> (true, acc)
        | ["don't()"; _ ; _] -> (false, acc)
        | [_; left; right] when canExecute -> (canExecute, (((left |> int) * (right |> int)) + acc))
        | _ -> (canExecute, acc)

[<TailCall>]
let rec filterInstructions canExecute acc (groups:Match list) = 
    let transformer = fun (x:Group) -> x.Value
    let ((isExecutable, newAcc), remainder) =
        match groups with
        | [head] -> (filterValid(mappedValues head transformer) canExecute acc), []
        | head :: tail -> (filterValid(mappedValues head transformer) canExecute acc), tail
        | _ -> (filterValid([]) canExecute acc), []                                            

    if remainder = [] then 
        newAcc
    else 
        filterInstructions isExecutable newAcc remainder
    
let processAllEnabledInstructions (input:string) =  
    let matches = Regex.Matches(input, @"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)|do\n\(\)") |> Seq.toList    
    filterInstructions true 0 matches

let processAllInstructions line =     
    seq {
        for m in Regex.Matches(line, @"mul\((\d{1,3}),(\d{1,3})\)") do        
            yield m.Groups[1].Value |> int, m.Groups[2].Value |> int
    }

let solveSingle (line:string) = 
    processAllInstructions line
    |> Seq.sumBy(fun (x, y) -> x * y)

let solve path = 
    let lines = 
        File.ReadLines(path)    
        |> Seq.toList
    
    lines
    |> Seq.map(solveSingle)
    |> Seq.sum
    |> printfn "%A"

    File.ReadAllText(path).Replace(Environment.NewLine, String.Empty)
    |> processAllEnabledInstructions
    |> printfn "%A"

open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task5-i.txt"

let input = File.ReadAllLines filename |> Seq.toList

let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines 

let rec parse (s: string) : (int64*int64) =
    let s = s.Split('-')
    s[0] |> int64, s[1] |> int64

let ranges = firstLines input |> List.map parse
let available =
    input
    |> List.skipWhile (fun line -> line <> "") |> List.tail
    |> List.map int64
    
printfn $"ranges: {ranges}"
printfn $"available: {available}"

let fresh i =
    ranges |> List.filter ( fun (a,b) -> i >= a && i <= b) |> List.isEmpty |> not 

available |> Set.ofList |> Set.toList |> List.filter fresh |> List.length |> printfn "Answer 1: %A"

let rec compress (ranges: (int64*int64) list) =
    match ranges with
    | [] -> []
    | [x] -> [x]
    | (a,b)::(_,d)::rest when d <= b -> compress ((a,b)::rest)
    | (a,b)::(c,d)::rest when c <= (b+1L) -> compress ((a,d)::rest)
    | (a,b)::(c,d)::rest when c <= b -> (a,b) :: compress ((b+1L,d)::rest)
    | (a,b)::rest -> (a,b) :: compress rest 
    
let distinct = ranges |> List.sort |> compress 

distinct |> List.map (printfn "%A")

let nums =
    distinct |> List.map (fun (a,b) -> b - a + 1L)
    |> List.sum 
    
nums |> printfn "Answer 2: %A"
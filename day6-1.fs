open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task6-t.txt"

let input = File.ReadAllLines filename |> Seq.toList

let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines 

let rec parse (s: string) : (int64*int64) =
    1,1 

let numbers: int64 list list =
    input |> List.take (List.length input - 1)
    |> List.map (fun s -> s.Split [|' '|])
    |> List.map Array.toList
    |> List.map (fun s -> s |> List.filter (fun s -> s <> ""))
    |> List.map (fun s -> s |> List.map int64)
    
let ops =
    input[List.length input - 1] |> _.ToCharArray()
    |> Array.toList |> List.filter (fun c -> c<>' ')            

numbers |> List.map (printfn "%A")

ops |>  (printfn "%A")

let rec cols (ops: char list) (numbers: int64 list list) =
    if ops = [] then []
    else
        let ncol = numbers |> List.map List.head
        let op = ops |> List.head 
        (op , ncol) :: cols (ops |> List.tail) (numbers |> List.map List.tail)
        
let eqs = cols ops numbers

let calcEq (op:char, numbers: int64 list) =
    match op with
    | '+' -> List.sum numbers
    | '*' -> numbers |> List.reduce (*)
 
let answers = eqs |> List.map calcEq

answers |> List.map (printfn "%A")

answers |> List.sum |> printfn "Answer 1: %A"
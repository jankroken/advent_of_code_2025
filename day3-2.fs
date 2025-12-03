open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task3-i.txt"

let input = File.ReadAllLines filename |> Seq.toList

let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines 

let rec parse (s:string) : int64 list =
    let toI (c:char) = c-'0' |> int64 
    s.ToCharArray() |> Array.toList |> List.map toI 

let banks = input |> List.map parse

let dig1 (l: int64 list) =
    l[0 .. l.Length - 2] |> List.max 

let dign (l: int64 list, n:int) =
    l[0 .. l.Length - (1+n)] |> List.max
    
let skip (l: int64 list, dig1:int64) =
    l |> List.skipWhile (fun c -> c <> dig1) |> List.tail 

let rec batteries (acc:int64) (n: int64 list, rem:int) =
    let dig = dign (n, rem)
    let rest = skip (n,dig)
    let acc = acc * 10L + dig
    if rem = 0 then acc
    else batteries acc (rest,rem-1)
    
let batts bank = batteries 0L (bank,11)

banks |> List.map batts |> List.map (printfn "batteries: %A")

banks |> List.map batts |> List.sum |> printfn "Answer 2: %A"
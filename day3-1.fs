open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task3-t.txt"

let input = File.ReadAllLines filename |> Seq.toList

let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines 

let rec parse (s:string) : int64 list =
    let toI (c:char) = c-'0' |> int64 
    s.ToCharArray() |> Array.toList |> List.map toI 

let banks = input |> List.map parse

let dig1 (l: int64 list) =
    l[0 .. l.Length - 2] |> List.max 

let dig2 (l: int64 list, dig1:int64) =
    l |> List.skipWhile (fun c -> c <> dig1) |> List.tail  |> List.max 

let charge (l:int64 list) =
    let d1 = dig1 l
    let d2 = dig2 (l,d1) 
    d1 * 10L + d2 

printfn $"{dig1 [1;2;3]}"
printfn $"{dig2 ([1;2;3],2)}"

banks |> List.map charge |> List.sum |> printfn "Answer 1: %A"
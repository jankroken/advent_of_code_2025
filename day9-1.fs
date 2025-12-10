open System
open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task9-t.txt"

let input = File.ReadAllLines filename |> Seq.toList

type Pos = int64*int64

let toPos (s:string) =
    let s = s.Split(',')
    s[0] |> int64, s[1] |> int64
    
let reds = input |> List.map toPos |> List.sort

reds |> List.map (printfn "%A")

let area ((x1,y1):Pos) ((x2,y2):Pos) : int64 =
    ((x2-x1 |> abs) + 1L) * ((y2-y1 |> abs) + 1L)

let rec maxArea (largest:int64) (reds:Pos list) =
    match reds with
    | [] -> largest
    | [_] -> largest
    | p1::rest ->
        let biggest = rest |> List.map (area p1) |> List.max 
        let largest = if biggest > largest then biggest else largest 
        maxArea largest rest 
        // areas |> List.max 
    
area (1L,1L) (2L,2L) |> printfn "%A"

let answer1 = maxArea 0L reds
answer1 |> printfn "Answer 1: %A"

let rec verts (acc:(Pos*Pos) list) (redsw:Pos list) =
    match redsw with
    | [] -> acc
    | [_] -> acc
    | (x1,_)::(x2,y2)::rest when x1 <> x2 -> verts acc ((x2,y2)::rest)
    | (x1,y1)::(x2,y2)::rest when y1 < y2 -> verts (((x1,y1),(x2,y2))::acc)  ((x2,y2)::rest)
    | (x1,y1)::(x2,y2)::rest when y1 >= y2 -> verts (((x2,y2),(x1,y1))::acc)  ((x2,y2)::rest)
        
let verticals = verts List.empty ([reds;[reds.Head]] |> List.concat) 
    
verticals |> List.map (printfn "%A")

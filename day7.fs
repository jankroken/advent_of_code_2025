open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task7-i.txt"

let input = File.ReadAllLines filename |> Seq.toList

let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines

let parse (y:int,s:string) =
    s.ToCharArray() |> Array.toList |> List.indexed |> List.map (fun (x,c) -> (x,y),c)

let map = input |> List.indexed |> List.map parse |> List.concat

let s =
    map |> List.filter (fun (_,c) -> c = 'S')
    |> List.head
    |> (fun (p,_) -> p)
    
let splitters =
    map |> List.filter (fun (_,c) -> c = '^')
    |> List.map (fun (p,_) -> p)
    |> Set.ofList
    
let space =
    map |> List.filter (fun (_,c) -> c = '.')
    |> List.map (fun (p,_) -> p)
    |> Set.ofList
    
let rec trace (beams: Set<int*int>) (newBeams:List<int*int>) =
    let expand (x:int,y: int) =
        if splitters.Contains (x,y+1) then [x-1,y+1;x+1,y+1] else [x,y+1]
    let nextBeams =
        newBeams |> List.map (fun c -> expand c)
        |> List.concat |> Set.ofList
        |> Set.filter space.Contains
        |> Set.toList
    let beams = Set.union beams (newBeams |> Set.ofList)
    if nextBeams = [] then beams else trace beams nextBeams

let beams = trace Set.empty [s]

let hitSplitters =
    splitters |> Set.map (fun (x,y) -> x,y-1)
    |> Set.filter beams.Contains

hitSplitters.Count |> printfn "Answer 1: %A"

let last_y = input.Length - 1

let rec calcTimes (exp_y: int) (timelines: Map<int*int,int64>) = 
    let news = space |> Set.filter (fun (x,y) -> y = exp_y)
    let alts (x,y) =
        let next = if splitters.Contains (x,y+1) then [x-1,y+1;x+1,y+1] else [x,y+1]
        let value x =
            match timelines.TryFind x with
            | None -> 1L
            | Some(t) -> t
        next |> List.map value |> List.sum
    let timelines =
        news |> Set.toList |> List.map (fun p -> p,alts p)
        |> List.fold (fun m (k,v) -> Map.add k v m) timelines 
    if (exp_y = 0) then timelines else
    calcTimes (exp_y-1) timelines
            
let timelines = calcTimes last_y Map.empty

timelines |> printfn "timelines: %A"

let below_s =
    let (x,y) = s
    (x,y+1)

timelines.TryFind (below_s) |> printfn "S: %A" 
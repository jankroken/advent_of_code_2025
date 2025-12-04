open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task4-i.txt"

let input = File.ReadAllLines filename |> Seq.toList

let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines 

let rec parse (y:int, s:string) : (int*int) list =
    let xcs = s.ToCharArray() |> Array.toList |> List.indexed
    xcs |> List.filter (fun (_,c) -> c = '@')
    |> List.map (fun (x,_) -> (x,y))
    
let rolls = input |> List.indexed |> List.map parse |> List.concat |> Set.ofList 

let neighbours (x,y) =
    [(x-1,y-1);(x,y-1);(x+1,y-1)
     (x-1,y);  (x+1,y)
     (x-1,y+1);(x,y+1);(x+1,y+1)]
    
let opens (rolls: Set<int*int>) =
    let available (roll: int*int) =
        let ns = neighbours roll |> List.filter rolls.Contains
        ns.Length < 4
    rolls |> Set.filter available  |> Set.count

opens rolls |> printfn "Anser 1: %A"

let tidy1 (rolls: Set<int*int>) =
    let available (roll: int*int) =
        let ns = neighbours roll |> List.filter rolls.Contains
        ns.Length < 4
    rolls |> Set.filter (fun roll -> available roll |> not)

let rec tidyAll rolls =
    let tidy = tidy1 rolls
    if tidy.Count = rolls.Count then tidy
    else tidyAll tidy

let tidied = tidyAll rolls

let removed = rolls.Count - tidied.Count

printfn $"Answer 2: {removed}"
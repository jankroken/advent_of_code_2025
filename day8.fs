open System
open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task8.txt"

let input = File.ReadAllLines filename |> Seq.toList 

let poss =
    let parse (s:string) =
        let s = s.Split(',')
        (s[0] |> int64, s[1] |> int64, s[2] |> int64)
    input |> List.map parse
    |> List.sort 
    
printfn $"{poss}"
        
let dist (x1,y1,z1) (x2,y2,z2) =
    Math.Sqrt (((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1)) |> double)
    
dist (162L,817L,812L) (425L,690L,689L) |> printfn "%A"
    
type Pos = int64*int64*int64
type Link = Pos*Pos*double
type Circuit = Set<Pos>

let Nope = (-1L,-1L,-1L)
let FarApart : Link = Nope,Nope,Double.MaxValue

let shortestDistance (found:Set<Pos*Pos>)(l: Pos list) =
    let rec closest (shortest: Link) (pos:Pos) (l:Pos list) : Link =
        let (_,_,shortestDistance) = shortest 
        match l with
        | [] -> shortest
        | p::rest ->
            let dist = if found.Contains(pos,p) then Double.MaxValue else dist pos p
            let shortest = if dist < shortestDistance then pos,p,dist else shortest
            if dist < (0L |> double) then printfn $"Overflow: {dist}"
            closest shortest pos rest
    let rec closestPair (shortest: Link) (l:Pos list) : Link =
        match l with
        | [] -> shortest
        | [_] -> shortest
        | p::rest ->
            let shortest = closest shortest p rest
            closestPair shortest rest
    closestPair FarApart l

shortestDistance Set.empty poss |> printfn "%A"

let fullyContains (circuit: Circuit) ((p1,p2):Pos*Pos) =
    circuit.Contains p1 && circuit.Contains p2

let toCircuit ((p1,p2):Pos*Pos) =
    (Set.empty.Add p1).Add p2 
    
let joinCircuits (circuits: Circuit list) =
    let rec join (joined:Circuit list) (circuits:Circuit list) =
        match circuits with
        | [] -> joined
        | [c] -> c::joined
        | c::rest ->
            let connected =
                rest |> List.filter (fun o -> Set.intersect c o |> Set.isEmpty |> not)
            if connected.IsEmpty then
                join (c::joined) rest
            else
                let rest = rest |> List.filter (fun o -> Set.intersect c o |> Set.isEmpty)
                let c = Set.unionMany (c::connected)
                join (c::joined) rest
    join List.empty circuits

let rec pairN (n:int) (circuits: List<Circuit>) (pairs:(Pos*Pos) Set) (l:Pos list) =
    printfn $"pairs: {pairs.Count}"
    if n = 1 then pairs
    else 
        let shortest = shortestDistance pairs poss
        let (p1,p2,_) = shortest
        let pairs = pairs.Add (p1,p2)
        let redundant = circuits
                        |> List.filter (fun c -> fullyContains c (p1,p2))
                        |> List.isEmpty |> not
        if redundant then 
           pairN n circuits pairs l
        else
           let circuit = toCircuit (p1,p2)
           let circuits = circuit::circuits |> joinCircuits
           pairN (n-1) circuits pairs l
           
        
let pairs = pairN 10 List.empty Set.empty poss

pairs |> printfn "%A"


let sweep (pairs: Set<Pos*Pos>) =
    let pairs = pairs |> Set.toList
                |> List.map (fun (p1,p2) -> (Set.empty.Add p1).Add p2)
    let rec sweep (circuit:Circuit) (skipped:Circuit list) (rest:Circuit list) =
        match rest with
        | [] -> circuit,skipped
        | p::rest when (Set.intersect circuit p).IsEmpty ->
            sweep circuit (p::skipped) rest
        | p::rest ->
            let circuit = Set.union circuit p
            sweep circuit skipped rest
    let rec sweepAll (swept: Circuit list) (circuits:Circuit list) =
        match circuits with
        | [] -> swept
        | c::rest ->
            let c,rest = sweep c List.empty rest
            sweepAll (c::swept) rest
    sweepAll List.empty pairs 

let circuits = sweep pairs
let sizes = circuits |> List.map (_.Count )

sizes |> printfn "%A"

let answer = sizes |> List.sort |> List.rev |> List.take 3 |> List.reduce (*)

printfn $"Answer 1: {answer}"

    
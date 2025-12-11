open System
open System.IO
open System.Diagnostics
open System.Linq

let filename = "/tmp/aoc/task11.txt"

let input = File.ReadAllLines filename |> Seq.toList

type Device = String * String list

let parse (s: String) : Device =
    let s = s.Split ':'
    let name = s[0]
    let connectors = s[1].Split ' ' |> Array.toList |> List.tail
    name, connectors

let devices = input |> List.map parse
let forwardMap = devices |> Map.ofList
let nodes =
    devices |> List.map fst
    |> List.filter (fun n -> n <> "you" && n <> "out")

let backMap =
    devices
    |> List.map (fun (m, cs) -> cs |> List.map (fun c -> c, m))
    |> List.concat
    |> List.groupBy fst
    |> List.map (fun (m, ins) -> m, ins |> List.map snd)
    |> Map.ofList

devices |> List.map (printfn "%A")

forwardMap |> printfn "Forward %A"

backMap |> printfn "Back: %A"

let rec canReach (reachable:string list) (frontier:string list) (nodes:string list) (map:Map<string,(string list)>) =
    printfn $"canReach reachable: {reachable.Length} {frontier.Length} {nodes.Length}"
    let next =
        frontier |> List.map map.TryFind
        |> List.filter _.IsSome
        |> List.map _.Value
        |> List.concat
        |> List.filter nodes.Contains
        |> List.distinct
    if next.IsEmpty then reachable |> List.distinct
    else 
        let reachable = [reachable;next] |> List.concat
        let nodes = nodes |> List.filter (fun s -> next.Contains s |> not)
        canReach reachable next nodes map
        

let forwardNodes = canReach List.empty ["you"] nodes forwardMap |> Set.ofList
let towardsEndNodes = canReach List.empty ["out"] nodes backMap |> Set.ofList
let reachableNodes = Set.intersect forwardNodes towardsEndNodes

printfn $"nodes={nodes.Length} reachable={reachableNodes.Count} endable={towardsEndNodes.Count} reachable={reachableNodes.Count}"

let rec filter (filtered:Device list) (devices:Device list) =
    match devices with
    | [] -> filtered
    | (node,conns)::rest when reachableNodes.Contains node || node = "you" ->
        let conns = conns |> List.filter (fun c -> c = "out" || c = "you" || reachableNodes.Contains c)
        let filtered = (node,conns)::filtered
        filter filtered rest
    | _::rest -> filter filtered rest
                
let network = filter List.empty devices |> Map.ofList

let pathNodes = network |> Map.keys |> Seq.toList 

printfn $"Network: {network}"

let InitCounts = ["out",1L] |> Map.ofList

let rec countPaths (skipped:String list) (pathsToOut:Map<String,int64>) (nodes:String list) =
    printfn $"countPaths skipped={skipped} costs={pathsToOut} nodes={nodes}"
    match nodes with
    | [] when skipped.IsEmpty -> pathsToOut
    | [] -> countPaths [] pathsToOut skipped 
    | node::rest ->
        printfn $"node = {node}"
        let deps = network[node]
        let costs = deps |> List.map pathsToOut.TryFind
        if costs.Contains None then
            let skipped = node::skipped
            countPaths skipped pathsToOut rest
        else
            let cost = costs |> List.map _.Value |> List.sum 
            let pathsToOut = pathsToOut.Add (node,cost)
            countPaths skipped pathsToOut rest
            
let costMap = countPaths List.empty InitCounts pathNodes

let youCost = costMap["you"]
printfn $"costMap[you] = {youCost}"
    
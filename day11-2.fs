open System
open System.IO
open System.Diagnostics

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
    devices |> List.map fst |> List.filter (fun n -> n <> "svr" && n <> "out")

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

let rec canReach
    (reachable: string list)
    (frontier: string list)
    (nodes: string list)
    (map: Map<string, (string list)>)
    =
//    printfn $"canReach reachable: {reachable.Length} {frontier.Length} {nodes.Length}"

    let next =
        frontier
        |> List.map map.TryFind
        |> List.filter _.IsSome
        |> List.map _.Value
        |> List.concat
        |> List.filter (fun n -> nodes |> List.contains n)
        |> List.distinct

    if next.IsEmpty then
        reachable |> List.distinct
    else
        let reachable = [ reachable; next ] |> List.concat
        let nodes = nodes |> List.filter (fun s -> next |> List.contains s |> not)
        canReach reachable next nodes map


let forwardNodes = canReach List.empty [ "svr" ] nodes forwardMap |> Set.ofList
let towardsEndNodes = canReach List.empty [ "out" ] nodes backMap |> Set.ofList
let reachableNodes = Set.intersect forwardNodes towardsEndNodes


// printfn $"nodes={nodes.Length} reachable={reachableNodes.Count} endable={towardsEndNodes.Count} reachable={reachableNodes.Count}"


let rec filter (filtered: Device list) (devices: Device list) =
    match devices with
    | [] -> filtered
    | (node, conns) :: rest when reachableNodes.Contains node || node = "svr" ->
        let conns =
            conns
            |> List.filter (fun c -> c = "out" || c = "svr" || reachableNodes.Contains c)

        let filtered = (node, conns) :: filtered
        filter filtered rest
    | _ :: rest -> filter filtered rest

let network = filter List.empty devices |> Map.ofList

let pathNodes = network |> Map.keys |> Seq.toList

// printfn $"Network: {network}"

// node+fft+dac
let InitCounts = [ ("out", false, false), 1L ] |> Map.ofList
let InitCounts4 =
    [ ("out", false, false), 1L
      ("out", true,false), 0L
      ("out", false, true), 0L
      ("out", true, true), 0L
       ] |> Map.ofList

let rec countPaths (skipped: String list) (pathsToOut: Map<String * bool * bool, int64>) (nodes: String list) =
//    printfn $"countPaths skipped={skipped} costs={pathsToOut} nodes={nodes}"

    match nodes with
    | [] when skipped.IsEmpty -> pathsToOut
    | [] -> countPaths [] pathsToOut skipped
    | node :: rest ->
//        printfn $"node = {node}"
        let deps = network[node]
        let costsNone = deps |> List.map (fun s -> pathsToOut.TryFind(s, false, false))
        let costsBoth = deps |> List.map (fun s -> pathsToOut.TryFind(s, true, true))
        let costsFFT = deps |> List.map (fun s -> pathsToOut.TryFind(s, true, false))
        let costsDAC = deps |> List.map (fun s -> pathsToOut.TryFind(s, false, true))
        let costsAll = [ costsNone; costsBoth; costsFFT; costsDAC ] |> List.concat

//        printfn $"costsAll = {costsAll}"
        
        if costsAll |> List.contains None then
            let skipped = node :: skipped
            countPaths skipped pathsToOut rest
        else
            match node with
            | "fft" ->
                let costFFT = costsNone |> List.map _.Value |> List.sum
                let costBoth = costsDAC |> List.map _.Value |> List.sum
                let pathsToOut = pathsToOut.Add ((node,true,false),costFFT)
                let pathsToOut = pathsToOut.Add ((node,true,true),costBoth)
                let pathsToOut = pathsToOut.Add ((node,false,false),0L)
                let pathsToOut = pathsToOut.Add ((node,false,true),0L)
                countPaths skipped pathsToOut rest
            | "dac" ->
                let costDAC = costsNone |> List.map _.Value |> List.sum
                let costBoth = costsFFT |> List.map _.Value |> List.sum
                let pathsToOut = pathsToOut.Add ((node,false,true),costDAC)
                let pathsToOut = pathsToOut.Add ((node,true,true),costBoth)
                let pathsToOut = pathsToOut.Add ((node,false,false),0L)
                let pathsToOut = pathsToOut.Add ((node,true,false),0L)
                countPaths skipped pathsToOut rest
            | node ->
                let costNone = costsNone |> List.map _.Value |> List.sum
                let costBoth = costsBoth |> List.map _.Value |> List.sum
                let costFFT = costsFFT |> List.map _.Value |> List.sum
                let costDAC = costsDAC |> List.map _.Value |> List.sum
                
                let pathsToOut = pathsToOut.Add ((node,false,false),costNone)
                let pathsToOut = pathsToOut.Add ((node,true,false),costFFT)
                let pathsToOut = pathsToOut.Add ((node,false,true),costDAC)
                let pathsToOut = pathsToOut.Add ((node,true,true),costBoth)
                countPaths skipped pathsToOut rest

let costMap = countPaths List.empty InitCounts4 pathNodes


// costMap |> Map.toList |> List.map (printfn "%A")

let youCost = costMap["svr",true,true]
printfn $"costMap[you] = {youCost}"

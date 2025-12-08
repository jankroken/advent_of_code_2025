open System
open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task8.txt"

let input = File.ReadAllLines filename |> Seq.toList

let poss =
    let parse (s: string) =
        let s = s.Split(',')
        (s[0] |> int64, s[1] |> int64, s[2] |> int64)

    input |> List.map parse |> List.sort

printfn $"{poss}"

let dist (x1, y1, z1) (x2, y2, z2) =
    Math.Sqrt(
        ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1))
        |> double
    )

dist (162L, 817L, 812L) (425L, 690L, 689L) |> printfn "%A"

type Pos = int64 * int64 * int64
type Link = Pos * Pos * double
type Circuit = Set<Pos>

let rec allDist (dists: Link list list) (poss: Pos list) : Link list =
    printfn "."

    match poss with
    | [] -> dists |> List.concat
    | p :: rest ->
        let pdist = rest |> List.map (fun p2 -> p, p2, dist p p2)
        allDist (pdist :: dists) rest

let dists =
    allDist List.empty poss
    |> List.sortBy (fun (_, _, dist) -> dist)
    |> List.map (fun (p1, p2, pos) -> (p1, p2))


printfn $"dists.size = {dists.Length}"

type SetMatch =
    | No
    | Single
    | Both

let matchPos (circuit: Circuit) ((p1, p2): Pos * Pos) =
    let c1 = circuit.Contains p1
    let c2 = circuit.Contains p2

    if c1 && c2 then Both
    elif c1 || c2 then Single
    else No

let toSet ((p1, p2): Pos * Pos) = Set.ofList [ p1; p2 ]

let rec linkN (circuits: Circuit list) (n: int) ((link :: rest): (Pos * Pos) list) =
    printfn $"linkN (circuits={circuits |> List.map _.Count} n={n} link={link}::..."
    let points = circuits |> List.map (fun c -> c, matchPos c link)
    let both = points |> List.filter (fun (_, m) -> m = Both)

    if both.Length > 0 then
        linkN circuits n rest
    else
        let circuit = toSet link
        let touches = points |> List.filter (fun (_, m) -> m = Single)

        if touches.IsEmpty then
            let circuits = circuit :: circuits
            linkN circuits n rest
        else
            let matchingCircuits = touches |> List.map (fun (link, _) -> link)
            let circuit = Set.unionMany (circuit :: matchingCircuits)

            let nonMatching =
                points |> List.filter (fun (_, m) -> m = No) |> List.map (fun (c, _) -> c)

            if circuit.Count = n then
                link
            else
                let circuits = circuit :: nonMatching
                linkN circuits n rest


let (x1,_,_),(x2,_,_) = linkN List.empty (poss.Length) dists
let answer2 = x1 * x2


printfn $"link = {answer2}"

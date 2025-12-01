open System.IO

let filename = "/tmp/aoc/task1-i.txt"

let input = File.ReadAllLines filename |> Seq.toList


let firstLines lines =
    List.takeWhile (fun line -> line <> "") lines 

let rec parse (s:string) : char*int64 =
    let c = s[0]
    let i = s[1..] |> int64
    c,i

let sum = input |> List.map parse

let rec solve (pos: int64) (turns: (char*int64) list) : int64 =
    match turns with
    | [] -> 0L
    | ('L', c):: rest ->
        let pos = (pos - c + 100L) % 100L
        let c = if pos = 0L then 1L else 0L
        c + (solve pos rest)
    | ('R', c):: rest ->
        let pos = (pos + c) % 100L
        let c = if pos = 0L then 1L else 0L
        c + (solve pos rest)

let rec posAndClicksLeft (c:int64) =
        let a = abs c
        let fulls = a / 100L
        printfn $"a={a} fulls={fulls}"
        0L

let rec solve2 (pos: int64) (turns: (char*int64) list) : int64 =
    match turns with
    | [] -> 0L
    | (_, 0L)::rest -> solve2 pos rest
    | ('L', c)::rest when pos = 0L ->
        solve2 99L (('L', c-1L)::rest)
    | ('L', c)::rest when c >= pos ->
        1L + solve2 0L (('L', c-pos)::rest)
    | ('L', c)::rest when pos > c ->
        solve2 (pos-c) rest
    | ('R', c)::rest when pos + c < 100L ->
        solve2 (pos+c) rest
    | ('R', c)::rest when pos + c >= 100L ->
        1L + solve2 0L (('R', c-(100L-pos))::rest)
    
        
printfn $"sum={sum}" 

let s = solve 50 sum

printfn $"solution 1: {s}"

let s2 = solve2 50 sum
printfn $"solution 2: {s2}"

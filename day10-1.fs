open System
open System.IO
open System.Diagnostics

let filename = "/tmp/aoc/task10.txt"

let input = File.ReadAllLines filename |> Seq.toList

type Switch = bool

type Button = int list
type Joltages = int list

let parseBounded (s:string) =
    let s = s.Substring (1, s.Length-2)
    s.Split(',') |> Array.toList |> List.map int
    
let parse (line:string) =
    let line = line.Split (' ') |> Array.toList 
    let indicators = line[0].ToCharArray()
                     |> Array.toList
                     |> List.filter (fun c -> c = '.' || c = '#')
                     |> List.map (fun c -> c <> '.')
                     |> List.toArray
    let buttons = line |> List.filter (fun s -> s.StartsWith '(')
                  |> List.map parseBounded 
    let joltages = line |> List.last |> parseBounded
    // printfn $"{indicators}"
    // printfn $"{wirings}"
    // printfn $"{joltages}"
    (indicators, buttons, joltages)

let machines = input |> List.map parse

let push (state:Switch array) (button:Button) =
    let state = Array.copy state // leave the original undestroyed
    let rec push (state:Switch array) (button:Button) =
        match button with
        | [] -> state
        | i::rest ->
            state[i] <- not state[i]
            push state rest
    push state button

let rec findButtonCombos (pushes:int64) (target:Switch array) (state:Switch array)  (buttons:Button list)=
    if target = state then pushes
    elif buttons.IsEmpty then Int64.MaxValue
    else
        let pushThisButton =
            let state = push state buttons.Head
            findButtonCombos (pushes+1L) target state buttons.Tail
        let dontPush =
            findButtonCombos pushes target state buttons.Tail
        min pushThisButton dontPush 
            
let solve (target:Switch array, buttons:Button list, _) =
    let state = target |> Array.map (fun c -> false)
    findButtonCombos 0L target state buttons

let answer1 = machines |> List.map solve |> List.sum

printfn $"Answer 1: {answer1}"
module Day23

open System
open System.Collections.Generic

let init () =
    () // not used

let cost (cat: char) dist = (pown 10 (int cat - int 'a')) * dist

let effectivePos colDepth (col, _) = 
    2 + (int col - int 'a')*colDepth

let isCol (cat, _) = cat <> 't'
let pos (_, c) = if Char.IsLetter c then 10 else int c - int '0'
let posChar i = if i = 10 then 'x' else char (int '0' + i)

let dist colDepth start target = 
    let colStart = 
        if not (isCol start) then 0
        else pos start
    let colTarget = 
        if not (isCol target) then 0
        else pos target
    let start = if isCol start then effectivePos colDepth start else pos start
    let target = if isCol target then effectivePos colDepth target else pos target
    if target > start then (target - start) + colStart + colTarget
    else (start - target) + colStart + colTarget

let costByDist colDepth cat start target = 
    dist colDepth start target |> cost cat

let parts (line: string) =
    line |> Seq.toArray |> Array.chunkBySize 2 |> Array.map (fun a -> a[0], a[1])

let blocked colDepth start target (line: string) =
    let sv = if isCol start then effectivePos colDepth start else pos start
    let tv = if isCol target then effectivePos colDepth target else pos target
    let line = parts line
    let topBlocked = 
        line 
        |> Seq.filter (isCol >> not)
        |> Seq.map pos
        |> Seq.exists (fun digit -> 
            (sv < tv && digit <= tv && digit > sv)
            || (tv < sv && digit < sv && digit >= tv))
    if topBlocked then true
    else
        if isCol start && Array.exists (fun p -> fst p = fst start && pos p < pos start) line then true
        else if isCol target && Array.exists (fun p -> fst p = fst target && pos p < pos target) line then true
        else false

let newLine (line: string) index (newCat: char, newPos: char) =
    if newCat = 'h' then
        failwith "invalid cat"
    line[0..(index*2)-1] + Char.ToString newCat + Char.ToString newPos + line[(index+1)*2..]

let deepestFree colDepth category positions =
    let allowedMin = (int category - int 'a') * colDepth
    let allowedMax = allowedMin + colDepth - 1
    let positions = Array.indexed positions
    if positions |> Array.exists (fun (i, (cat, _)) -> 
        cat = category && (i < allowedMin || i > allowedMax)) then
        None
    else
        [colDepth..(-1)..1] 
        |> List.tryFind (fun i -> 
            not (positions |> Array.exists(fun (_, p) -> fst p = category && pos p = i)))
        |> Option.map (fun i -> category, posChar i)

let next colDepth (line: string) =
    let positions = parts line
    [|0..positions.Length-1|] |> Array.collect (fun index ->
        let cat = char ((index / colDepth) + int 'a')
        let target = deepestFree colDepth cat positions
        let c = positions[index]
        if isCol c then
            match target with
            | Some t when fst t = fst c && pos c > pos t -> Array.empty // in right position
            | Some t when not (blocked colDepth c t line) ->
                [|costByDist colDepth cat c t, newLine line index t|] // move direct to right position
            | _ -> // move up top
                parts "t0t1t3t5t7t9tx"
                |> Array.filter (fun target -> not (blocked colDepth c target line))
                |> Array.map (fun target -> 
                    costByDist colDepth cat c target, newLine line index target)
        else
            match target with
            | None -> Array.empty
            | Some t when blocked colDepth c t line -> Array.empty
            | Some t ->
                [|costByDist colDepth cat c t, newLine line index t|])

let getGoal colDepth =
    Array.concat
        [|
            Array.create colDepth 'a'
            Array.create colDepth 'b'
            Array.create colDepth 'c'
            Array.create colDepth 'd'
        |]
    |> String
    
let lineComplete line goal = 
    line 
    |> Seq.filter (fun c -> Char.IsLetter c && c <> 'x')
    |> Seq.toArray
    |> String
    |> fun res -> res = goal

let nextMemo = Dictionary<string, (int * string)[]>()

let nextMemoised colDepth (line: string) =
    if nextMemo.ContainsKey line then nextMemo[line]
    else
        let res = next colDepth line
        nextMemo.Add(line, res)
        res

let visited = HashSet<int * string>()

let rec minToWin goal colDepth lowestCost options = 
    if Array.isEmpty options then lowestCost
    else
        let (cost, line:string) = Array.head options
        //printfn "%s" line
        let rem = Array.tail options
        if not (visited.Add((cost, line))) || cost > lowestCost then
            minToWin goal colDepth lowestCost rem
        else if lineComplete line goal then
            minToWin goal colDepth cost rem
        else
            let options = nextMemoised colDepth line
            let nextOptions = 
                options 
                |> Array.choose (fun (optionCost, line) -> 
                    if visited.Contains(cost + optionCost, line) then
                        None
                    else if cost + optionCost < lowestCost then 
                        Some (cost + optionCost, line) 
                    else None)
            let allOptions = Array.append nextOptions rem |> Array.sortBy fst
            minToWin goal colDepth lowestCost allOptions

let part1 () =
    
    let start = "b1d2a2c2b2d1a1c1"
    let colDepth = 2

    let goal = getGoal colDepth
    
    nextMemo.Clear()
    visited.Clear()
    minToWin goal colDepth Int32.MaxValue [|0, start|]

let part2 () =
    
    //let start = "b1c3d2d4a4b3c2c4a2a3b4d1a1b2d3c1"
    let start = "a4c3d2d4"+"a1b3c1c2"+"b1b2c4d3"+"a2a3b4d1"
    let colDepth = 4

    let goal = getGoal colDepth

    nextMemo.Clear()
    visited.Clear()
    minToWin goal colDepth Int32.MaxValue [|0, start|]
    //parts start
    //nextMemoised colDepth "a4t5d2d4c2b3txt9b1b2c4c3a2a3b4d1"
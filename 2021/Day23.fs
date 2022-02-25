module Day23

open System
open System.Collections.Generic

// need to remake this to be more adaptive to longer cols
// switch from 1 char to two? T and index, A/B/C/D and index?
// bDACBdac becomes B1D2A2C2B2D1A1C1
// target would be A1A2B1B2C1C2D1D2 or AABBCCDD with numbers removed

//let processed = "ADacbCBd" // test data
let processed = "bDACBdac" // my data

let init () =
    () // not used

let part1 () =

    let start = "b1d2a2c2b2d1a1c1"
    let colDepth = 2

    let cost (cat: char) dist = (pown 10 (int cat - int 'a')) * dist

    let effectivePos (col, _) = 
        2 + (int col - int 'a')*2
    
    let isCol (cat, _) = cat <> 't'
    let pos (_, c) = if Char.IsLetter c then 10 else int c - int '0'
    let posChar i = if i = 10 then 'x' else char (int '0' + i)

    let dist start target = 
        let colStart = 
            if not (isCol start) then 0
            else pos start
        let colTarget = 
            if not (isCol target) then 0
            else pos target
        let start = if isCol start then effectivePos start else pos start
        let target = if isCol target then effectivePos target else pos target
        if target > start then (target - start) + colStart + colTarget
        else (start - target) + colStart + colTarget

    let costByDist cat start target = 
        dist start target |> cost cat
    
    let parts (line: string) =
        line |> Seq.toArray |> Array.chunkBySize 2 |> Array.map (fun a -> a[0], a[1])

    let blocked start target (line: string) =
        let sv = if isCol start then effectivePos start else pos start
        let tv = if isCol target then effectivePos target else pos target
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
        line[0..(index-1)*2] + Char.ToString newCat + Char.ToString newPos + line[(index+1)*2..]

    let nextMemo = Dictionary<string, (int * string)[]>()

    let next (line: string) =
        let parts = parts line
        [|0..7|] |> Array.collect (fun index ->
            let cat = char ((index / 2) + int 'a')
            let c = parts[index]
            if isCol c then
                if fst c = cat && [pos c..colDepth] |> List.forall (fun i -> Array.contains (cat, posChar i) parts) then Array.empty // current index is in right place
                else if not (blocked c upper line) then 
                    [|costByDist index c upper, newLine line index upper|]
                else if not (blocked c targetCol line) then
                    [|costByDist index c targetCol, newLine line index targetCol|]
                else 
                    [|'0';'1';'3';'5';'7';'9';'x'|]
                    |> Array.filter (fun target -> not (blocked c target line))
                    |> Array.map (fun target -> 
                        costByDist index c target, newLine line index target)
            else
                if blocked c targetCol line then Array.empty
                else
                    if not (line.Contains(Char.ToString upper)) then 
                        [|costByDist index c upper, newLine line index upper|] // col is empty
                    else if line.Contains(Char.ToString targetCol) then Array.empty // col is full
                    else if (index % 2 = 0 && line[index + 1] = upper) || (index % 2 = 1 && line[index - 1] = upper) then 
                        [|costByDist index c targetCol, newLine line index targetCol|] // top of col is target
                    else Array.empty)

    let nextMemoised (line: string) =
        if nextMemo.ContainsKey line then nextMemo[line]
        else
            let res = next line
            nextMemo.Add(line, res)
            res

    let visited = HashSet<int * string>()

    let rec minToWin lowestCost options = 
        if Array.isEmpty options then lowestCost
        else
            let (cost, line:string) = Array.head options
            let rem = Array.tail options
            if not (visited.Add((cost, line))) || cost > lowestCost then
                minToWin lowestCost rem
            else if line.ToLower() = "aabbccdd" then
                minToWin cost rem
            else
                let options = nextMemoised line
                let nextOptions = 
                    options 
                    |> Array.choose (fun (optionCost, line) -> 
                        if visited.Contains(cost + optionCost, line) then
                            None
                        else if cost + optionCost < lowestCost then 
                            Some (cost + optionCost, line) 
                        else None)
                minToWin lowestCost (Array.append nextOptions rem)

    minToWin Int32.MaxValue [|0, start|]

let part2 () =
    0
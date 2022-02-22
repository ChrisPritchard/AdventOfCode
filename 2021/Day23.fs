module Day23

open Common
open System

let processed = 
    let lines = readEmbedded "day23"
    [|
        [|lines[2][3];lines[3][3]|]
        [|lines[2][5];lines[3][5]|]
        [|lines[2][7];lines[3][7]|]
        [|lines[2][9];lines[3][9]|]
    |]

let init () =
    processed |> Array.length |> ignore

let part1 () =

    // render input as a sequence of numbers in hex, from 0 to 14 (E)
    // or as 0-6 and then AaBbCcDd?
    // also in order - there are eight units we are concerned about
    // so starting data is (for test): ADacbCBd
    // next state in test is ADa3bCBd

    let cost index dist = (pown 10 (index / 2)) * dist

    let effectivePos col = 
        let neutral = Char.ToLower col
        2 + (int neutral - int 'a')*2
    
    let isCol c = Char.IsLetter c && c <> 'x'
    let top c = if Char.IsLetter c then 10 else int c - int '0'

    let dist start target = 
        let colStart = 
            if not (isCol start) then 0
            else if Char.IsUpper start then 2
            else 1
        let colTarget = 
            if not (isCol target) then 0
            else if Char.IsUpper target then 2
            else 1
        let start = if isCol start then effectivePos start else top start
        let target = if isCol target then effectivePos target else top target
        if target > start then (target - start) + colStart + colTarget
        else (start - target) + colStart + colTarget

    let costByDist index start target = 
        dist start target |> int |> cost index

    let blocked start target (line: string) =
        let sv = if isCol start then effectivePos start else top start
        let tv = if isCol target then effectivePos target else top target
        let topBlocked = 
            line 
            |> Seq.filter (isCol >> not)
            |> Seq.map top
            |> Seq.exists (fun digit -> 
                (sv < tv && digit <= tv && digit > sv)
                || (tv < sv && digit < sv && digit >= tv))
        if topBlocked then false
        else
            if Char.IsUpper start && line.Contains (Char.ToLower start) then false
            else if Char.IsUpper target && line.Contains (Char.ToLower target) then false
            else if isCol target && line.Contains target then false
            else true

    let newLine (line: string) index (newPos: char) =
        line[0..index-1] + Char.ToString newPos + line[index+1..]

    let next (line: string) =
        [|0..7|] |> Array.collect (fun index ->
            let targetCol = char ((index / 2) + int 'a')
            let upper = Char.ToUpper targetCol
            let c = line[index]
            if isCol c then
                if c = Char.ToUpper targetCol then Array.empty // current index is in right place
                else if c = targetCol && index % 2 = 1 && line[index - 1] = Char.ToUpper targetCol then Array.empty // ditto
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
                    else if line.IndexOf (Char.ToString upper) = index / 2 then 
                        [|costByDist index c targetCol, newLine line index targetCol|] // top of col is target
                    else Array.empty)

        // if up then
            // test path to col, if blocked then no target
            // test if target col is empty, if so then thats a target
            // else test if its full if so then no targets
            // else test if its bottom element is in right position, if so then first is target
            // else no targets
        // if down then
            // if in right position, then nothing
            // each up not blocked should be tested
            // for each check if path to position is blocked
            // additionally

    next "ADacbCBd" // should go to ADa3bCBd
    // next "ADa3cCBd" // should go to ADa3bCBd
    //dist 'c' '3'

let part2 () =
    
    0
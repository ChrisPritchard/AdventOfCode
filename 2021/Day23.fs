module Day23

open Common
open System
open System.Collections.Generic

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
        if topBlocked then true
        else
            if Char.IsUpper start && line.Contains (Char.ToLower start) then true
            else if Char.IsUpper target && line.Contains (Char.ToLower target) then true
            else if isCol target && line.Contains target then true
            else false

    let newLine (line: string) index (newPos: char) =
        line[0..index-1] + Char.ToString newPos + line[index+1..]

    let next (line: string) =
        [|0..7|] |> Array.collect (fun index ->
            let targetCol = char ((index / 2) + int 'a')
            let upper = Char.ToUpper targetCol
            let c = line[index]
            if isCol c then
                if c = Char.ToUpper targetCol then Array.empty // current index is in right place
                else if c = targetCol && ((index % 2 = 0 && line[index + 1] = Char.ToUpper targetCol) || (index % 2 = 1 && line[index - 1] = Char.ToUpper targetCol)) then Array.empty // ditto
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

    let memo = Dictionary<string, int option>()

    let rec minToWin line = 
        if memo.ContainsKey line then memo[line]
        else
            if line.ToLower() = "aabbccdd" then
                memo.Add(line, Some 0)
                Some 0
            else
                let options = next line
                let minOptions = options |> Array.choose (fun (cost, nextLine) -> minToWin nextLine |> Option.map (fun nextCost -> nextCost + cost))
                let res = if Array.isEmpty minOptions then None else Some (Array.min minOptions)
                memo.Add(line, res)
                res

    // minToWin "ADacbCBd" |> Option.get // test data
    // minToWin "bCACBdac" |> Option.get // my data
    minToWin "CbACBdac" |> Option.get // my data

let part2 () =
    
    0
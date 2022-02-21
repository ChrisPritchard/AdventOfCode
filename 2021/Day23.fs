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

    let cost index dist = (pown 10 (index / 2)) * dist

    let effectivePos col = 
        let neutral = Char.ToLower col
        1.5 + float (int neutral - int 'a')
    
    let dist start target = 
        let col = 
            if Char.IsUpper start || Char.IsUpper target then 1.5
            else 0.5
        let start = if Char.IsLetter start then effectivePos start else float start - float '0'
        let target = if Char.IsLetter target then effectivePos target else float target - float '0'
        if target > start then (target - start) + col
        else (start - target) + col

    let costByDist index start target = 
        dist start target |> int |> cost index

    let blocked start target line =
        let start = if Char.IsLetter start then effectivePos start else float start
        let target = if Char.IsLetter target then effectivePos target else float target
        line 
        |> Seq.filter Char.IsDigit
        |> Seq.map (fun c -> float  c - float '0')
        |> Seq.exists (fun digit -> 
            (start < target && digit <= target && digit > start)
            || (target < start && digit < start && digit >= target))

    let newLine (line: string) index (newPos: char) =
        line[0..index-1] + Char.ToString newPos + line[index+1..]

    let next (line: string) =
        [|0..7|] |> Array.collect (fun index ->
            let targetCol = char ((index / 2) + int 'a')
            let c = line[index]
            if Char.IsLetter c then
                if c = Char.ToUpper targetCol then Array.empty // current index is in right place
                else if c = targetCol && index % 2 = 1 && line[index - 1] = Char.ToUpper targetCol then Array.empty // ditto
                else 
                    [|'0'..'6'|] 
                    |> Array.filter (fun target -> not (blocked c target line))
                    |> Array.map (fun target -> 
                        costByDist index c target, newLine line index target)
            else
                if blocked c targetCol line then Array.empty
                else
                    let upper = Char.ToUpper targetCol
                    if not (line.Contains(Char.ToString upper)) then 
                        [|costByDist index c upper, newLine line index upper|] // col is empty
                    else if line.Contains(Char.ToString targetCol) then Array.empty // col is full
                    else if line.IndexOf (Char.ToString upper) = index / 2 then 
                        [|costByDist index c targetCol, newLine line index targetCol|] // top of col is target
                    else Array.empty)

        // collect options for each char, track with cost and new line
        // collect options for each new line
        // ultimately if a line is a success then it returns cost

        // let options = collect options.
        // if options empty, then none
        // else options choose next options, min
        // memoize result?
        // could be line -> options -> map to next -> min or none

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

    next "ADacbCBd"

let part2 () =
    
    0
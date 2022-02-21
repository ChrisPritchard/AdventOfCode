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

    let blocked start target line =
        let start = if Char.IsLetter start then effectivePos start else float start
        let target = if Char.IsLetter target then effectivePos target else float target
        line 
        |> Seq.filter Char.IsDigit
        |> Seq.map (fun c -> float  c - float '0')
        |> Seq.exists (fun digit -> 
            (start < target && digit <= target && digit > start)
            || (target < start && digit < start && digit >= target))

    let dist start target = 
        let col = 
            if Char.IsUpper start || Char.IsUpper target then 1.5
            else 0.5
        let start = if Char.IsLetter start then effectivePos start else float start
        let target = if Char.IsLetter target then effectivePos target else float target
        if target > start then (target - start) + col
        else (start - target) + col

    let next (line: string) =
        []

        // collect options for each char, track with cost and new line
        // collect options for each new line
        // ultimately if a line is a success then it returns cost
        // let options = collect options.
        // if options empty, then none
        // else options choose next options, min
        // memoize result?

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

        // each given start state can calculate options, the cost to reach said option, and then recurse, trying to find the min cost
        

        // check each pair, keeping track of index
        // for each pair, calculate positions, index determines where it should be, up or col determines what section

    // the result of each possible move can be run as a new line with a cost
    // victory is simply matching A0A1B0B1C0C1D0D1    
    0

let part2 () =
    
    0
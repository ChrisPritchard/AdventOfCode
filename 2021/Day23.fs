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
    // any unit can move to one of seven locations from its start
    //  unless blocked: either by a unit in front or by another unit in one of the seven locations
    // once in the seven, a unit may move to only one location: its own column. it may only move here if its empty, 
    //  or if it has one of its kind inside

    // brute force all options to track victories, keeping record of paths so costs can be calculated
    // nodes could be presented as positions, with possible options available for each
    // this could also be memoised -> from a given setup, given costs?
    // could be presented as a mere set of 8 number pairs for AABBCCDD, 
    //  each presented as 0 to 6 for top positions, 0, 1 for col positions, 
    //  prefixed with T for top or the col home for bottom (e.g. A0)

    // "A0D0A1C1B1C0B0D1" for test data
    // could be done with pure ints for index, using 4 for up
    // "0030012111201031"

    // things can move are all ups, all 1s, and all 0s where there isnt also a 1
    // line can be parsed into columns for this calculations, or parsed as is

    let next (line: string) =
        let bits = line |> Seq.toArray
        [|0..3|]
        |> Array.collect (fun i ->
            let pos = line[i*2]
            if pos = 'T' then
                let targets = 
                    if line.Contains(string(i) + "1" then Array.empty
                    else if line[i*2]))

        // if up then
            // test path to col, if blocked then no target
            // test if target col is empty, if so then thats a target
            // else test if its full if so then no targets
            // else test if its bottom element is in right position, if so then first is target
            // else no targets
        // if down then
            // each up not blocked should be tested
            // for each check if path to position is blocked
        

        // check each pair, keeping track of index
        // for each pair, calculate positions, index determines where it should be, up or col determines what section

    // the result of each possible move can be run as a new line with a cost
    // victory is simply matching A0A1B0B1C0C1D0D1    
    0

let part2 () =
    
    0
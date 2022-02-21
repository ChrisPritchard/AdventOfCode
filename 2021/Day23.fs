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
        let elems = line |> Seq.toArray |> Array.chunkBySize 2 |> Array.map (fun a -> a[0], a[1]-'0')
        [|0..3|]
        |> Array.collect (fun i ->
            let (pos,idx) = elems[i]
            if pos = 'T' then
                let goal = i*2+2
                let blocked = elems |> Array.exists (fun j -> 
                    j <> i && line[j*2] = 'T' 
                    && let p = line[j*2+1]-'0' in 
                        if space < goal then p > space && p < goal else p > goal && p < space)
                if blocked then Array.empty
                else
                    let empty = elems |> Array.forall (fun (pos2,_) -> pos2 <> i)
                    if empty then 
                        // return dist cost + new board state
                        [||]
                    else
                        let blocked2 = elems |> Array.exists (fun (pos2,idx2) -> pos2 = i && idx = 1)
                        if blocked2 then Array.empty
                        else
                            let blocked3 = elems |> Array
                )

        // if up then
            // test path to col, if blocked then no target
            // test if target col is empty, if so then thats a target
            // else test if its full if so then no targets
            // else test if its bottom element is in right position, if so then first is target
            // else no targets
        // if down then
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
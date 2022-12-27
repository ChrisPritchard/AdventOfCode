module Day21

open Common
open System
open System.Collections.Generic

let part1() =

    let resolved = Dictionary<string, int64>()
    let unresolved = Dictionary<string, string * char * string>()
    for line in readEmbeddedRaw "day21" do
        let parts = split ": " line
        if parts.Length = 2 then
            resolved[parts[0]] <- Int64.Parse parts[1]
        else
            unresolved[parts[0]] <- parts[1], parts[2][0], parts[3]

    while not (resolved.ContainsKey "root") do
        for kv in unresolved do
            let first, op, second = kv.Value
            if resolved.ContainsKey first && resolved.ContainsKey second then
                // if kv.Key = "root" then
                //     printfn "%d %d" resolved[first] resolved[second]
                unresolved.Remove kv.Key |> ignore
                let resolvedValue = 
                    match op with
                    | '+' -> resolved[first] + resolved[second]
                    | '-' -> resolved[first] - resolved[second]
                    | '*' -> resolved[first] * resolved[second]
                    | '/' -> resolved[first] / resolved[second]
                    | _ -> failwithf "unknown op %A" op
                resolved.Add(kv.Key, resolvedValue)

    resolved["root"]

let part2() =
        
    let resolved = Dictionary<string, int64>()
    let unresolved = Dictionary<string, string * char * string>()
    for line in readEmbeddedRaw "day21" do
        let parts = split ": " line
        if parts[0] <> "humn" then
            if parts.Length = 2 then
                resolved[parts[0]] <- Int64.Parse parts[1]
            else
                unresolved[parts[0]] <- parts[1], parts[2][0], parts[3]

    let mutable unresolvedCount = unresolved.Count
    let mutable finished = false
    while not finished do
        for kv in unresolved do
            let first, op, second = kv.Value
            if resolved.ContainsKey first && resolved.ContainsKey second then
                unresolved.Remove kv.Key |> ignore
                let resolvedValue = 
                    match op with
                    | '+' -> resolved[first] + resolved[second]
                    | '-' -> resolved[first] - resolved[second]
                    | '*' -> resolved[first] * resolved[second]
                    | '/' -> resolved[first] / resolved[second]
                    | _ -> failwithf "unknown op %A" op
                resolved.Add(kv.Key, resolvedValue)
        if unresolvedCount = unresolved.Count then
            finished <- true
        else
            unresolvedCount <- unresolved.Count

    for kv in unresolved do
        let first, op, second = kv.Value
        let first = if resolved.ContainsKey first then resolved[first].ToString() else first
        let second = if resolved.ContainsKey second then resolved[second].ToString() else second
        unresolved[kv.Key] <- first, op, second
    
    // let reverse =
    //     function
    //     | '+' -> '-'
    //     | '-' -> '+'
    //     | '/' -> '*'
    //     | '*' -> '/'
    //     | _ -> '#'

    let rec findHumn value key =
        if key = "humn" then value
        else
            let first, op, second = unresolved[key]
            // printfn "\nnext op is '%d (%s) = %s %c %s'" value key first op second
            if Char.IsLetter first[0] then
                // nextKey op constant = value
                let constant = Int64.Parse second
                let nextKey = first
                // printfn "reversing to %s = %d %c %d" nextKey constant (reverse op) value
                match op with
                | '+' -> findHumn (value - constant) nextKey // value - constant = nextKey
                | '-' -> findHumn (value + constant) nextKey // k - 4 = 6 -> 6 + 4 = k  // value + constant = nextKey
                | '*' -> findHumn (value / constant) nextKey // value / constant = nextKey
                | '/' -> findHumn (value * constant) nextKey // value * constant = nextKey
                | _ -> failwithf "unknown op %A" op
            else
                // constant op nextKey = value
                let constant = Int64.Parse first
                let nextKey = second
                // printfn "reversing to %s = %d %c %d" nextKey value (reverse op) constant
                match op with
                | '+' -> findHumn (value - constant) nextKey // 4 + k = 6 -> 6 - 4 = k
                | '-' -> findHumn (constant - value) nextKey // 4 - k = 6 -> 4 - 6 = k
                | '*' -> findHumn (value / constant) nextKey // 4 * k = 6
                | '/' -> findHumn (constant / value) nextKey // (4 / k = 6) = (6 * k = 4) = (4 / 6 = k)
                | _ -> failwithf "unknown op %A" op

    let rootFirst,_, rootSecond = unresolved["root"]
    if Char.IsLetter rootFirst[0] then
        // printfn "root is %s = %s" rootFirst rootSecond
        findHumn (Int64.Parse rootSecond) rootFirst
    else
        // printfn "root is %s = %s" rootFirst rootSecond
        findHumn (Int64.Parse rootFirst) rootSecond
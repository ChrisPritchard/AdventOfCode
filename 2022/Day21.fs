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
    
    // now need to calculate the input number (for key 'humn') that will result in root's two keys being the same
    // this might be brute forcable, it might not be...
    
    0
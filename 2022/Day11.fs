module Day11

open Common
open System

type Monkey = {
    id: int
    items: bigint list
    operation: bigint -> bigint
    test: bigint
    onTrue: int
    onFalse: int
    inspections: int64
}

let parseAsMonkeys () =
    let monkeys, lastMonkey = 
        (([], None), readEmbeddedRaw "day11")
        ||> Seq.fold (fun (monkeys, current) line ->
            match current with
            | None ->
                monkeys, Some { id = List.length monkeys; items = []; operation = id; test = bigint 0; onTrue = 0; onFalse = 0; inspections = 0 }
            | Some monkey ->
                if line = "" then
                    monkey::monkeys, None
                else if line.StartsWith "  Starting items: " then
                    let items = line.Substring("  Starting items: ".Length) |> split ", " |> Array.map bigint.Parse |> List.ofArray
                    monkeys, Some { monkey with items = items }
                else if line.StartsWith "  Operation: new = old " then
                    let op = line.Substring("  Operation: new = old ".Length) |> split " "
                    let op =
                        if op[0] = "+" then
                            if op[1] = "old" then fun n -> n + n else fun n -> n + (bigint.Parse(op[1]))
                        else
                            if op[1] = "old" then fun n -> n * n else fun n -> n * (bigint.Parse(op[1]))
                    monkeys, Some { monkey with operation = op }
                else if line.StartsWith "  Test: divisible by " then
                    let test = line.Substring("  Test: divisible by ".Length) |> bigint.Parse
                    monkeys, Some { monkey with test = test }
                else if line.StartsWith "    If true: throw to monkey " then
                    let ifTrue = line.Substring("    If true: throw to monkey ".Length) |> Int32.Parse
                    monkeys, Some { monkey with onTrue = ifTrue }
                else if line.StartsWith "    If false: throw to monkey " then
                    let ifFalse = line.Substring("    If false: throw to monkey ".Length) |> Int32.Parse
                    monkeys, Some { monkey with onFalse = ifFalse }
                else
                    monkeys, Some monkey)
    let monkeys = 
        match lastMonkey with Some m -> m::monkeys | _ -> monkeys
        |> List.rev
    monkeys

let monkeyBusiness roundCount reduceWorry =
    let appendEntry item mID tosses =
        match Map.tryFind mID tosses with
        | None -> Map.add mID [item] tosses
        | Some l -> Map.add mID (item::l) tosses

    ((parseAsMonkeys (), Map.empty), [1..roundCount])
    ||> Seq.fold (fun (monkeys, tosses) _ ->
        (tosses, monkeys) 
        ||> List.mapFold (fun tosses monkey -> 
            let items, tosses = 
                match Map.tryFind monkey.id tosses with
                | None -> monkey.items, tosses
                | Some r -> List.append monkey.items (List.rev r), Map.remove monkey.id tosses
            let tosses = 
                (tosses, items)
                ||> List.fold (fun tosses item ->
                    let r = (monkey.operation item)
                    let r = if reduceWorry then r / (bigint 3) else r
                    if r % monkey.test = bigint 0 then appendEntry r monkey.onTrue tosses
                    else appendEntry r monkey.onFalse tosses)
            { monkey with inspections = monkey.inspections + int64 (List.length items); items = [] }, tosses))
    |> fst |> Seq.map (fun m -> m.inspections) |> Seq.sortByDescending id // |> Seq.take 2 |> Seq.reduce (*)

let part1 () =
    monkeyBusiness 20 true
    
let part2 () =
    monkeyBusiness 10000 false
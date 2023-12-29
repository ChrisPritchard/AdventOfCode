let input = System.IO.File.ReadAllLines "input.txt"

let mutable workflows = Map.empty
let mutable ratings = []

type Rule = LessThan of index:int * value:int * Next | GreaterThan of index:int * value:int * Next
and Next = Workflow of string | Accepted | Rejected

let next_for s = if s = "R" then Rejected else if s = "A" then Accepted else Workflow s

for line in input do
    if line.Length > 2 then
        if line[0] = '{' then
            let vals = line.Split ("{xmas=,}".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
            ratings <- vals::ratings
        else 
            let parts = line.Split ("{,}".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
            let key = parts[0]
            let default_action = next_for parts[parts.Length - 1]
            let rules = parts[1..parts.Length-2] |> Array.map (fun p -> 
                let index = "xmas".IndexOf p[0]
                let to_compare = int p[2..p.IndexOf ":"-1]
                let result = p[p.IndexOf ":"+1..]
                match p[1] with
                | '<' -> LessThan (index, to_compare, next_for result)
                | _ -> GreaterThan (index, to_compare, next_for result))
            workflows <- Map.add key (List.ofArray rules, default_action) workflows

let rec evaluate (rating: int []) workflow = 
    let rules, default_result = workflows[workflow]
    let rec check_rules rules =
        match rules with
        | [] -> default_result
        | GreaterThan (index, to_check, next)::rem ->
            if rating[index] > to_check then next
            else check_rules rem
        | LessThan (index, to_check, next)::rem ->
            if rating[index] < to_check then next
            else check_rules rem
    match check_rules rules with
    | Accepted -> Array.sum rating
    | Rejected -> 0
    | Workflow s ->
        evaluate rating s

let part1 = ratings |> List.sumBy (fun r -> evaluate r "in")
printfn "Part 1: %d" part1

// start with full range
// for a given rule we need to calculate a split, less than vs greater than
// we could work out all values that are rejected, those get added to totals, then remove those from 4000

let empty () = [|0;0;0;0|]

let rec all_failures (min: int[]) (max: int[]) workflow =
    let rules, default_result = workflows[workflow] 
    let acc = [|0;0;0;0|]
    let rec evaluate = function
        | [] -> ()
        | next::rem ->
            match next with
            | LessThan (index, to_check, next) ->
                match next with 
                | Rejected -> 
                    acc[index] <- acc[index] + (to_check - min[index])
                | Workflow s ->
                    let res = all_failures min max s
                    for (i, v) in Array.indexed res do
                        acc[i] <- acc[i] + v
                | _ -> ()
                min[index] <- to_check
                evaluate rem
            | GreaterThan (index, to_check, next) ->
                match next with 
                | Rejected -> 
                    acc[index] <- acc[index] + (max[index] - to_check)
                | Workflow s ->
                    let res = all_failures min max s
                    for (i, v) in Array.indexed res do
                        acc[i] <- acc[i] + v
                | _ -> ()
                max[index] <- to_check
                evaluate rem
    evaluate rules
    match default_result with 
    | Rejected -> 
        for (i, v) in Array.indexed acc do
            acc[i] <- acc[i] + (max[i] - min[i])
    | Workflow s ->
        let res = all_failures min max s
        for (i, v) in Array.indexed res do
            acc[i] <- acc[i] + v
    | _ -> ()
    acc

let failures = all_failures [|0;0;0;0|] [|4000;4000;4000;4000|] "test"
printfn "%A" failures

let part2 = 0
printfn "Part 2: %d" part2
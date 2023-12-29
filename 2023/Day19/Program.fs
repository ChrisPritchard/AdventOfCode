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

let rec all_failures (min: uint64[]) (max: uint64[]) workflow =
    let get_total_except index = [0..3] |> List.except [index] |> List.map (fun i -> (max[i] - min[i]) + 1UL) |> List.reduce (*)
    let array_copy_but (a: uint64[]) index change = [|0..3|] |> Array.map (fun i -> if i <> index then a[i] else change a[i])

    let rules, default_result = workflows[workflow] 
    let mutable total_failures = 0UL
    for rule in rules do
        match rule with 

        | GreaterThan (index, to_check, next) ->
            if max[index] > uint64 to_check then
                match next with
                | Rejected ->
                    total_failures <- total_failures + (get_total_except index * (max[index] - uint64 to_check))
                    max[index] <- uint64 to_check
                | Accepted -> 
                    max[index] <- uint64 to_check
                | Workflow s ->
                    let new_min = array_copy_but min index (fun v -> uint64 (to_check + 1))
                    total_failures <- total_failures + all_failures new_min (Array.copy max) s
                    max[index] <- uint64 to_check

        | LessThan (index, to_check, next) ->
            if min[index] < uint64 to_check then
                match next with
                | Rejected ->
                    total_failures <- total_failures + (get_total_except index * (uint64 to_check - min[index]))
                    min[index] <- uint64 to_check
                | Accepted -> 
                    min[index] <- uint64 to_check
                | Workflow s ->
                    let new_max = array_copy_but max index (fun v -> uint64 (to_check - 1))
                    total_failures <- total_failures + all_failures (Array.copy min) new_max s
                    min[index] <- uint64 to_check

    match default_result with
    | Rejected ->
        total_failures <- total_failures + (get_total_except -1)
    | Accepted -> ()
    | Workflow s ->
        total_failures <- total_failures + all_failures (Array.copy min) (Array.copy max) s

    total_failures

let failures = all_failures [|1UL;1UL;1UL;1UL|] [|4000UL;4000UL;4000UL;4000UL|] "in"
let part2 = (pown 4000UL 4) - failures
printfn "Part 2: %d" part2
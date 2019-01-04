
open System
open System.IO

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"

    let people, likeLevels =
        input
        |> Array.fold (fun (people, likeLevels) line ->
            let line = line.Split [|' ';'.'|]
            let newPeople = Set.add line.[0] people
            let amount = Int32.Parse line.[3]
            let amount = if line.[2] = "lose" then -1 * amount else amount
            let newLikeness = Map.add (line.[0], line.[10]) amount likeLevels
            newPeople, newLikeness)
            (Set.empty, Map.empty)

    let people = Set.toList people

    let rec combinations people soFar =
        match people with
        | [] -> [soFar]
        | _ ->
            people 
            |> List.collect (fun p -> 
                combinations (List.filter ((<>) p) people) (p::soFar))
    
    let score (likeLevels: Map<string * string, int>) combination = 
        let len = List.length combination
        [0..len - 1] |> List.sumBy (fun i -> 
            let left = if i = 0 then len-1 else i - 1
            let right = if i = len-1 then 0 else i + 1
            likeLevels.[combination.[i], combination.[left]]
            + likeLevels.[combination.[i], combination.[right]])

    let part1 = combinations people [] |> List.map (score likeLevels) |> List.max
    printfn "part 1: %i" part1

    let likeLevelsWithMe = 
        people 
        |> List.fold (fun levels p -> 
            levels
            |> Map.add ("me", p) 0
            |> Map.add (p, "me") 0
        ) likeLevels
    let peopleWithMe = "me"::people

    let part2 = combinations peopleWithMe [] |> List.map (score likeLevelsWithMe) |> List.max
    printfn "part 2: %i" part2

    0
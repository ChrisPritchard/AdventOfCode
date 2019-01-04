open System.IO
open System

type Reindeer = { name: string; speed: int; flyTime: int; restTime: int; distance: int; state: State }
and State = Flying of int | Resting of int

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let parseReindeer (line: string) =
        let line = line.Split [|' '|]
        {
            name = line.[0]
            speed = Int32.Parse line.[3]
            flyTime = Int32.Parse line.[6]
            restTime = Int32.Parse line.[13]
            distance = 0
            state = Flying 0
        }

    let reindeer = input |> Array.map parseReindeer
    let part2Scores = reindeer |> Array.map (fun r -> r.name, 0) |> Map.ofArray
    
    let postFlightReindeer, part2Scores =
        [1..2503] |> List.fold (fun (reindeer, scores) _ ->
            let newReindeer = 
                reindeer 
                |> Array.map (fun r ->
                    match r.state with
                    | Flying n ->
                        let dist = r.distance + r.speed
                        if n + 1 = r.flyTime then { r with distance = dist; state = Resting 0 }
                        else { r with distance = dist; state = Flying (n + 1) }
                    | Resting n ->
                        if n + 1 = r.restTime then { r with state = Flying 0 }
                        else { r with state = Resting (n + 1) })

            let currentLeaders = 
                newReindeer 
                |> Array.groupBy (fun r -> r.distance) 
                |> Array.sortByDescending fst 
                |> Array.head 
                |> snd
            let newScores = 
                currentLeaders 
                |> Array.fold (fun scores r -> 
                    Map.add r.name (Map.find r.name scores + 1) scores) 
                    scores 

            newReindeer, newScores) (reindeer, part2Scores)

    let part1 = postFlightReindeer |> Array.map (fun r -> r.distance) |> Array.max
    printfn "part 1: %i" part1

    let part2 = part2Scores |> Map.toList |> List.maxBy snd |> snd
    printfn "part 2: %i" part2

    0

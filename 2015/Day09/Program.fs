open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let cities, travelTimes =
        input
        |> Array.fold (fun (cities, travelTimes) line ->
            let line = line.Split [|' '|]
            let newCity = Set.add line.[0] cities
            let andCity = Set.add line.[2] newCity
            let newTime = Map.add (line.[0], line.[2]) (Int32.Parse line.[4]) travelTimes
            let andTime = Map.add (line.[2], line.[0]) (Int32.Parse line.[4]) newTime
            andCity, andTime)
            (Set.empty, Map.empty)
    let cities = Set.toList cities

    let rec findShortestTime =
        function
        | [] | [_] -> 0
        | [city;other] -> travelTimes.[city, other]
        | city::rest ->
            rest |> List.map (fun other -> travelTimes.[city, other] + findShortestTime (other::(List.except [other] rest))) |> List.min

    let part1 = cities |> List.map (fun city -> findShortestTime (city::(List.except [city] cities))) |> List.min
    printfn "part 1: %i" part1

    let rec findLongestTime =
        function
        | [] | [_] -> 0
        | [city;other] -> travelTimes.[city, other]
        | city::rest ->
            rest |> List.map (fun other -> travelTimes.[city, other] + findLongestTime (other::(List.except [other] rest))) |> List.max

    let part2 = cities |> List.map (fun city -> findLongestTime (city::(List.except [city] cities))) |> List.max
    printfn "part 2: %i" part2

    0
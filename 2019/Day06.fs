module Day06

open Common
open System.IO

let lines = File.ReadAllLines ("./inputs/day06.txt")

let part1 () =
    let orbits = lines |> Array.map (fun s -> (s.Split ')').[1], (s.Split(')').[0]))
    let orbitMap = orbits |> Map.ofArray
    let rec counter acc c =
        if not (Map.containsKey c orbitMap) then 
            acc
        else
            counter (acc + 1) orbitMap.[c]
    orbits |> Array.sumBy (fst >> counter 0)

let part2 () =
    let orbits = lines |> Array.map (fun s -> (s.Split ')').[1], (s.Split(')').[0]))
    let orbitMap = orbits |> Map.ofArray
    BFS.bfs 
        (fun s -> orbitMap.["SAN"] = s) 
        (fun s -> 
            let orbiting = orbits |> Array.filter (snd >> (=) s) |> Array.map fst
            if Map.containsKey s orbitMap 
            then orbiting |> Array.append [|orbitMap.[s]|] |> Seq.ofArray
            else orbiting |> Seq.ofArray)
        orbitMap.["YOU"]
    |> Option.defaultValue [] |> Seq.length |> fun i -> i - 1
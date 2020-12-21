module Day21

open Common
open System.IO

let input = File.ReadAllLines "./inputs/day21.txt"

let processed () = 
    input
    |> Array.map (fun line ->
        line
        |> splitOn " (contains "
        |> fun a -> splitOn " " a.[0], split " ,)" a.[1])

let ingredientAllergies lines =
    let candidates = 
        lines 
        |> Array.collect (fun (recipe, allergens) -> 
            allergens |> Array.map (fun allergen -> allergen, Set.ofArray recipe))
        |> Array.groupBy fst
        |> Array.map (fun (allergen, sets) -> allergen, sets |> Array.map snd |> Array.reduce Set.intersect)
        |> Array.sortBy (snd >> Set.count)
        |> List.ofArray
    let rec reducer acc rem = 
        match rem with
        | [] -> acc
        | (allergen, options)::rem ->
            if Set.count options <> 1 then failwithf "assertion failed: %s: %A" allergen options
            let ingredient = Set.toArray options |> Array.head
            let rem = rem |> List.map (fun (ing, opts) -> ing, Set.remove ingredient opts) |> List.sortBy (snd >> Set.count)
            reducer (Map.add ingredient allergen acc) rem
    reducer Map.empty candidates

let part1 () = 
    let lines = processed ()
    let map = ingredientAllergies lines
    let all = lines |> Array.collect fst |> Array.distinct
    let others = all |> Array.filter (fun ing -> not (Map.containsKey ing map))
    lines |> Array.sumBy (fun (recipe, _) ->
        others |> Array.filter (fun o -> Array.contains o recipe) |> Array.length)

let part2 () =
    let lines = processed ()
    let map = ingredientAllergies lines
    map |> Map.toArray |> Array.sortBy snd |> Array.map fst |> String.concat ","
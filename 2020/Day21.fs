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

let part1 () = 
    let lines = processed ()
    let all = lines |> Array.collect fst |> Array.distinct
    let candidates = 
        lines 
        |> Array.collect (fun (recipe, allergens) -> 
            allergens |> Array.map (fun allergen -> allergen, Set.ofArray recipe))
        |> Array.groupBy fst
        |> Array.map (fun (allergen, sets) -> allergen, sets |> Array.map snd |> Array.reduce Set.intersect)
        |> Array.sortBy (snd >> Set.count)
    let map =
        (Map.empty, candidates)
        ||> Array.fold (fun acc (allergen, options) ->
            let ingredient = options |> Set.filter (fun ing -> not (Map.containsKey ing acc)) |> Set.toArray
            //if ingredient.Length <> 1 then failwithf "assertion failed: %s: %A" allergen ingredient
            Map.add ingredient.[0] allergen acc)
    let others = all |> Array.filter (fun ing -> not (Map.containsKey ing map))
    lines |> Array.sumBy (fun (recipe, _) ->
        others |> Array.filter (fun o -> Array.contains o recipe) |> Array.length)

    // for each ingredient
    // find recipes with that ingredient and intersect
    // intersections are options
    // if single option, exclude from variables

    // map to ing -> line
    // group by ing
    // map to set intersect lines
    // sort by intersect size
    // fold to construct ingredient candidates

let part2 () = 0
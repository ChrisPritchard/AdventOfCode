module Day13

open Common
open System
open System.IO

let input = File.ReadAllLines "./inputs/day13.txt"

let processed () = 
    let earliest = int input.[0]
    let ids = input.[1] |> split ","
    earliest, ids

let part1 () =
    let earliest, ids = processed ()
    ids 
    |> Array.filter (fun id -> id <> "x")
    |> Array.map (fun id ->
        let id = int id
        if earliest % id = 0 then id, earliest 
        else
            let sub = earliest / id
            id, (sub + 1) * id)
    |> Array.minBy snd
    |> fun (id, time) -> (time - earliest) * id
    
// technique from LizTheGrey again: https://www.twitch.tv/videos/835702252
// based on the idea that, given all the numbers are prime, you can 'preserve' 
// an earlier calulated modulus with offset via multiplying by the last product
// i dont know... magic maths. should look up the euclidian theorem

let part2 () =
    let _, ids = processed()
    let indexed = 
        ids 
        |> Array.indexed 
        |> Array.filter (fun (_, id) -> id <> "x") 
        |> Array.map (fun (i, id) -> uint64 i, uint64 id)
        |> List.ofArray

    let rec nextMin min index id prod =
        if (min + index) % id = 0UL then min
        else nextMin (min + prod) index id prod 

    let rec searcher min prod numbers =
        match numbers with
        | [] -> min
        | (index, id)::rest ->
            let min = nextMin min index id prod
            let prod = prod * id
            searcher min prod rest

    searcher 0UL 1UL indexed
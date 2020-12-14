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
// for a given id, and offset, you find a sample where sample + offset % id = 0
// once you have that number, the first key insight is adding the id value to it, however many times, will result in the same result
// e.g. 8 % 7 = 1, (8+7) % 7 = 1, (8+ (7*100)) % 7 = 1
// the second key inside is that this 'adding seven' could be multiplied by the next id, once you find the sample for that id:
//   e.g. say 7 is the first number, 13 is the second, the product for the third is 91. from the first insight above, adding 7 thirteen times 
//   will result in the same mod 1. if for the 13 id the result is mod 2, adding 13 seven times will still be mod 2
// so by making the 'step' while we search for the next number + offset to get mod 0 (see the first line of this desc) the product of all prior ids
//   we ensure that our number, when found, will still satisfy all the constraints we have tested for so far

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
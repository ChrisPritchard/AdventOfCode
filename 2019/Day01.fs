module Day01

open System.IO

let input = File.ReadAllLines ("./inputs/day01.txt")

let part1 () =
    input |> Array.sumBy (float >> fun v -> floor (v / 3.) - 2.) |> int

let part2 () =

    let rec processFuel acc v = 
        let res = floor (v / 3.) - 2.
        if res <= 0. then acc
        else processFuel (acc + res) res

    input |> Array.sumBy (float >> processFuel 0.) |> int
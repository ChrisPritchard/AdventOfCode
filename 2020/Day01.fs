module Day01

open System.IO

let input = File.ReadAllLines ("./inputs/day01.txt")

let part1 () =
    input 
    |> Array.collect (fun a -> input |> Array.map (fun b -> int a, int b))
    |> Array.find(fun (a, b) -> a + b = 2020)
    |> fun (a, b) -> a * b

let part2 () =
    input 
    |> Array.collect (fun a -> 
        input |> Array.collect (fun b -> 
            input |> Array.map (fun c -> int a, int b, int c)))
    |> Array.find(fun (a, b, c) -> a + b + c = 2020)
    |> fun (a, b, c) -> a * b * c

    // let rec processFuel acc v = 
    //     let res = floor (v / 3.) - 2.
    //     if res <= 0. then acc
    //     else processFuel (acc + res) res

    // input |> Array.sumBy (float >> processFuel 0.) |> int
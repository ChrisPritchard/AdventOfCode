module Day25

open Common

let part1 () =
    
    let input = readEmbedded "day25"

    let sum = 
        input
        |> Array.sumBy (fun line ->
            Seq.rev line 
            |> Seq.indexed 
            |> Seq.sumBy (fun (i, c) -> 
                let value = 
                    match c with
                    | '=' -> -2
                    | '-' -> -1
                    | '0' -> 0
                    | '1' -> 1
                    | '2' | _ -> 2
                value * (pown 5 i)))

    // go through each number and convert it to snafu
    // when adding snafu together, there needs to be an overflow system....
    // or convert from other direction?

    sum
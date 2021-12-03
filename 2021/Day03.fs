module Day03

open Common

let processed = readEmbedded "day03" |> Array.map (fun s -> split " " s |> fun a -> a[0], int a[1])

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let mutable x = 0
    let mutable y = 0
    for (command, amt) in processed do
        if command = "down" then
            y <- y + amt
        else if command = "up" then
            y <- y - amt
        else
            x <- x + amt
    x * y

let part2 () =
    0

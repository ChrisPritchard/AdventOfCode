module Day02

open Common

let processed = readEmbedded "day02" |> Array.map (fun s -> split " " s |> fun a -> a[0], int a[1])

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
    let mutable x = 0
    let mutable y = 0
    let mutable aim = 0
    for (command, amt) in processed do
        let parts = split " " command
        if command = "down" then
            aim <- aim + amt
        else if command = "up" then
            aim <- aim - amt
        else
            x <- x + amt
            y <- y + aim * amt
    x * y

module Day02

open Common

let processed = readEmbedded "day02"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let mutable x = 0
    let mutable y = 0
    for command in processed do
        let parts = split " " command
        if parts[0] = "down" then
            y <- y + int parts[1]
        else if parts[0] = "up" then
            y <- y - int parts[1]
        else
            x <- x + int parts[1]
    x * y

let part2 () =
    let mutable x = 0
    let mutable y = 0
    let mutable aim = 0
    for command in processed do
        let parts = split " " command
        if parts[0] = "down" then
            aim <- aim + int parts[1]
        else if parts[0] = "up" then
            aim <- aim - int parts[1]
        else
            x <- x + int parts[1]
            y <- y + aim * int parts[1]
    x * y


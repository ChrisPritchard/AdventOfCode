open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections

// Solved this by replicating Tribulu's C++ answer here: https://www.reddit.com/r/adventofcode/comments/a53r6i/2018_day_11_solutions/ebjogd7
// Barely understand how this works, but flexing F#'s imperative muscles was a bit of a change

let power sn x y =
    let rackId = x + 10
    let power = ((rackId * y) + sn) * rackId
    let hundreds = power / 100 % 10
    hundreds - 5

let sumTable sn = 
    let sums = Array2D.zeroCreate<int> 301 301
    for x = 1 to 300 do
        for y = 1 to 300 do
            sums.[y,x] <- power sn x y + sums.[y - 1, x] + sums.[y, x - 1] - sums.[y - 1, x - 1]
    sums

let maxRect (sumTable : int [,]) size =
    let mutable best, bx, by = 0, 0, 0
    for x = size to 300 do
        for y = size to 300 do
            let total = 
                sumTable.[y, x] - sumTable.[y - size, x] - 
                sumTable.[y, x - size] + sumTable.[y - size, x - size]
            if total > best then
                best <- total
                bx <- x
                by <- y
    best, bx - size + 1, by - size + 1

[<EntryPoint>]
let main _ =
    let input = 8772

    let sums = sumTable input
    let part1 = maxRect sums 3
    let (_, x, y) = part1

    printfn "part 1: %i,%i" x y

    let part2 = 
        [1..300] 
        |> Seq.map (fun s -> s, maxRect sums s) 
        |> Seq.maxBy (fun (_, (t, _, _)) -> t)
    let (s, (_, x, y)) = part2
    
    printfn "part 2: %i,%i,%i" x y s

    0
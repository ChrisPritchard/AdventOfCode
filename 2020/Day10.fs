module Day10

open System
open System.IO
open Common

let input = File.ReadAllLines "./inputs/day10.txt"

let processed () =
    let raw = input |> Array.map int |> Array.sortBy id
    Array.concat
        [|
            [|0|]
            raw
            [|raw.[Array.length raw - 1]+3|]
        |]

let part1 () =
    let a = processed ()
    let rec counter i ones threes =
        if i = a.Length then
            ones * threes
        else
            let diff = a.[i] - a.[i-1]
            if diff = 1 then
                counter (i+1) (ones+1) threes
            elif diff = 3 then
                counter (i+1) ones (threes+1)
            else
                counter (i+1) ones threes
    counter 1 0 0

// I figured out the explanation for part2 via LizTheGrey on Twitch https://www.twitch.tv/videos/832019096
// her code is here: https://github.com/lizthegrey/adventofcode/blob/main/2020/day10.go

// basically at each index, calculate the ways it can reach the end
// this is done via a sum of the points it can reach, and *their* prior caclulated sum
// which means you need to start at the end - whose value is 1, as it can reach the end 1 way
// walk back: point n-1 can reach the end 1 way. point n-2 if its less or equal to 3 can reach the end direct or via n-1, so its val is 2
// point n-3 can reach the end if its 3 or less direct, via n-1 or via n-2, so 1+1+2
// and so on until you get to the start

let part2 () =
    let a = processed ()
    let rec memer i sums =
        if i < 0 then
            Map.find 0 sums
        else
            let n = a.[i]
            let sum =
                [i+1..i+3] 
                |> List.sumBy (fun j -> 
                    if Map.containsKey j sums && a.[j] - a.[i] <= 3 then sums.[j] else 0L)
            memer (i-1) (Map.add i sum sums)
            
    // start at the end - 1, as the end is already calculated at 1
    memer (a.Length - 2) (Map.empty.Add (a.Length - 1, 1L))
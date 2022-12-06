module Day06

open Common
open System

let part1And2 () =
    let line = (readEmbedded "day06")[0]
    let findPattern n =
        line |> Seq.windowed n |> Seq.map (Set.ofSeq >> Seq.length) |> Seq.findIndex (fun s -> s = n) |> (+) n
    findPattern 4, findPattern 14

// original

// let part1And2 () =
//     let line = (readEmbedded "day06")[0]
//     let endOfHeader = 
//         [4..line.Length] |> Seq.find (fun i -> 
//             line[(i-4)..i-1] |> Set.ofSeq |> Seq.length |> (=) 4)
//     let endOfMessage = 
//         [14..line.Length] |> Seq.find (fun i -> 
//             line[(i-14)..i-1] |> Set.ofSeq |> Seq.length |> (=) 14)
//     endOfHeader, endOfMessage


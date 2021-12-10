module Day10

open Common
open System

let processed = readEmbedded "day10"

let init () =
    processed |> Array.length |> ignore

type Res = 
    | Closed
    | Continue of char list
    | Result of char

let part1 () =

    let beg c = Array.contains c [|'(';'[';'<';'{'|]
    let opp = function
    | ')' -> '('
    | ']' -> '['
    | '>' -> '<'
    | _ -> '{'
    let scores = Map.ofArray [|')',3;']',57;'}',1197;'>',25137|]
    let rec score queue rem =
        match rem with
        | [] -> 0
        | c::rem when beg c ->
            score (c::queue) rem
        | c::rem ->
            match queue with
            | o::queue when o = opp c ->
                score queue rem
            | _ -> 
                scores[c]

    processed
    |> Array.sumBy (fun s -> score [] (List.ofSeq s))

let part2 () =
    0

// let test =
//     let s = "c7afbc7a3ed8f0c719cd134fc7330241b186e53e97fe4a8056321812f51972f1"
//     let t = "60b868"

//     let md5 = System.Security.Cryptography.MD5.Create ()

//     let hex (bytes: seq<byte>) =
//         bytes
//         |> Seq.map (fun b -> Convert.ToString(b, 16).PadLeft(2, '0'))
//         |> String.concat ""

//     let hexMd5Hash (s: string) = 
//         let bytes = (s.ToCharArray ()) |> Array.map byte
//         let hash = md5.ComputeHash (bytes) |> Array.map (fun b -> Convert.ToString(b, 16).PadLeft(2, '0'))
//         (String.concat "" hash).ToLower()

//     let rec finder i = 
//         let p = s + string i
//         let res = hexMd5Hash p
//         if res.StartsWith t then
//             string i
//         else
//             finder (i + 1)

//     finder 0

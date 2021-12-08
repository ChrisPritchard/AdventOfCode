module Day08

open Common
open System

let processed = readEmbedded "day08" |> Array.map (fun l -> let parts = split "|" l in split " " parts[0], split " " parts[1])

let init () =
    processed |> Array.length |> ignore

let part1 () =
    processed
    |> Array.collect snd
    |> Array.countBy String.length
    |> Array.sumBy (fun (l, c) -> if l = 2 || l = 4 || l = 3 || l = 7 then c else 0)

let part2 () =

    let characters = Map.ofList [
        Set.ofList ['a';'b';'c';'e';'f';'g'], '0'
        Set.ofList ['c';'f'], '1'
        Set.ofList ['a';'c';'d';'e';'g'], '2'
        Set.ofList ['a';'c';'d';'f';'g'], '3'
        Set.ofList ['b';'c';'d';'f'], '4'
        Set.ofList ['a';'b';'d';'f';'g'], '5'
        Set.ofList ['a';'b';'d';'e';'f';'g'], '6'
        Set.ofList ['a';'c';'f'], '7'
        Set.ofList ['a';'b';'c';'d';'e';'f';'g'], '8'
        Set.ofList ['a';'b';'c';'d';'f';'g'], '9'
    ]    

    let translate mappings =
        
        let sorted = Array.sortBy String.length mappings |> Array.map (fun s -> s.ToCharArray() |> Set.ofArray)
        let cORf = sorted[0]
        let a = Set.difference sorted[1] cORf
        let bORd = Set.difference sorted[2] cORf
        let fives = sorted[3..5]
        let (b,g) = 
            let target = fives |> Array.find (fun o -> Set.isSubset cORf o)
            let d = Set.difference bORd target
            let g = Set.difference target (Set.unionMany [a; cORf; bORd])
            d, g
        let d = Set.difference bORd b
        let (c,f) =
            let sub = Set.unionMany [a; b; d; g]
            let target = fives |> Array.find (fun o -> Set.isSubset sub o)
            let f = Set.difference target sub
            let c = Set.difference cORf f
            c, f
        let e = Set.difference sorted[9] (Set.unionMany [a; b; c; d; f; g])

        let translator = Map.ofList [
            (Set.toArray a)[0], 'a'
            (Set.toArray b)[0], 'b'
            (Set.toArray c)[0], 'c'
            (Set.toArray d)[0], 'd'
            (Set.toArray e)[0], 'e'
            (Set.toArray f)[0], 'f'
            (Set.toArray g)[0], 'g'
        ]
        mappings 
        |> Array.map (fun s -> 
            let translated = 
                s.ToCharArray() 
                |> Array.map (fun c -> translator[c])
                |> Set.ofArray
            let number = characters[translated]
            Set.ofSeq s, number)
        |> Map.ofArray

    let decode (mappings, message: string[]) =
        let key = translate mappings
        message 
        |> Array.map (fun s -> key[Set.ofSeq s])
        |> asString
        |> int

    processed
    |> Array.sumBy decode

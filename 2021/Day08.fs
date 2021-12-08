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

        let asChars (s: string) = s.ToCharArray()
        let asSet = asChars >> Set.ofArray 
        
        let sorted = Array.sortBy String.length mappings |> Array.map asSet

        let cORf = sorted[0]
        let a = Set.difference sorted[1] cORf
        let bORd = Set.difference sorted[2] cORf
        let fives = sorted[3..5]
        let (b,g) = 
            let target = Array.find (Set.isSubset cORf) fives
            let d = Set.difference bORd target
            let g = Set.difference target (Set.unionMany [a; cORf; bORd])
            d, g
        let d = Set.difference bORd b
        let (c,f) =
            let sub = Set.unionMany [a; b; d; g]
            let target = Array.find (Set.isSubset sub) fives
            let f = Set.difference target sub
            let c = Set.difference cORf f
            c, f
        let e = Set.difference sorted[9] (Set.unionMany [a; b; c; d; f; g])

        let gv = Set.toArray >> Array.head
        let translator = Map.ofList [
            gv a, 'a'
            gv b, 'b'
            gv c, 'c'
            gv d, 'd'
            gv e, 'e'
            gv f, 'f'
            gv g, 'g'
        ]

        mappings 
        |> Array.map (fun s -> 
            let translated = 
                asChars s
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

    Array.sumBy decode processed

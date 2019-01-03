open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let mutable map = Map.empty<string, unit -> uint16>

    let parseToken (token: string) = 
        match UInt16.TryParse token with
        | (true, i) -> fun () -> i
        | (false, _) -> fun () -> Map.find token map ()

    let parseInstruction (s: string) =
        let bits = s.Split [|' '|]
        let inst = bits.[0..bits.Length - 3]

        let func =
            if inst.Length = 1 then
                parseToken inst.[0]
            else if inst.[0] = "NOT" then
                fun () -> ~~~((parseToken inst.[1]) ())
            else if inst.[1] = "AND" then
                fun () -> 
                    (parseToken inst.[0]) ()
                    &&&
                    (parseToken inst.[2]) ()
            else if inst.[1] = "OR" then
                fun () -> 
                    (parseToken inst.[0]) ()
                    |||
                    (parseToken inst.[2]) ()
            else if inst.[1] = "LSHIFT" then
                fun () -> 
                    (parseToken inst.[0]) ()
                    <<< 
                    Int32.Parse inst.[2]
            else // RSHIFT
                fun () -> 
                    (parseToken inst.[0]) ()
                    >>>
                    Int32.Parse inst.[2]
                    
        map <- Map.add (Array.last bits) func map

    input |> Array.iter parseInstruction
    
    let part1 = map.["a"] ()
    printfn "part 1: %i" part1

    0

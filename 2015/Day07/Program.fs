open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let mutable program = Map.empty<string, unit -> uint16>
    let mutable cache = Map.empty<string, uint16>

    let parseToken (token: string) = 
        match UInt16.TryParse token with
        | (true, i) -> fun () -> i
        | (false, _) -> fun () -> 
            match Map.tryFind token cache with
            | Some v -> v
            | None ->
                let v = Map.find token program ()
                cache <- Map.add token v cache
                v

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
                    
        program <- Map.add (Array.last bits) func program

    input |> Array.iter parseInstruction
    
    let part1 = program.["a"] ()
    printfn "part 1: %i" part1

    cache <- ["b", part1] |> Map.ofList
    let part2 = program.["a"] ()
    printfn "part 2: %i" part2

    0

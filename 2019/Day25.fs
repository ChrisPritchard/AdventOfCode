module Day25

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllText "./inputs/day25.txt" |> split ","

let part1 () =

    let io = Intcode.IO.create ()

    let rec runner ip rb mem instructions =
        match instructions with
        | instruction::remaining ->
            instruction 
            |> Seq.iter (int64 >> io.write)
            io.write 10L

            let _, ip, rb, mem = Intcode.run ip rb mem io
            
            let rec reader acc =
                let canRead, result = io.read ()
                if canRead then 
                    reader ((char result)::acc)
                else
                    List.rev acc |> asString
            let response = reader []
            if remaining = [] then response, ip, rb, mem
            else
                runner ip rb mem remaining
        | _ -> 
            "", ip, rb, mem

    let instructions = [
        "east"
        "north"
        "north"
        "take spool of cat6"
        "south"
        "east"
        "take mug"
        "north"
        "north"
        "west"
        "take asterisk"
        "south"
        "take monolith"
        "north"
        "east"
        "south"
        "east"
        "take sand"
        "south"
        "west"
        "take prime number"
        "east"
        "north"
        "east"
        "south"
        "take tambourine"
        "west"
        "take festive hat"
        "north"
        "inv"
    ]

    let _, ip, rb, mem = runner 0L 0L (Intcode.memFrom input) instructions

    let finalInv = [
        "prime number"
        "spool of cat6"
        "festive hat"
        "monolith"
        "mug"
        "asterisk"
        "sand"
        "tambourine"
    ]

    let (_, ip, rb, mem) =
        (("", ip, rb, mem), finalInv) 
        ||> List.fold (fun (_, ip, rb, mem) inv ->
            runner ip rb mem ["drop " + inv])

    let rec options acc available = 
        seq {
            yield acc
            for other in available do
                yield! options (Set.add other acc) (Set.remove other available) 
        }
    
    let allOptions = options Set.empty (Set.ofList finalInv) |> Seq.toList |> List.distinct

    let rec tester ip rb mem options =
        match options with
        | set::remaining ->
            let (_, ip, rb, mem) =
                (("", ip, rb, mem), Set.toArray set) 
                ||> Array.fold (fun (_, ip, rb, mem) inv ->
                    runner ip rb mem ["take " + inv])
            let response, ip, rb, mem = runner ip rb mem ["west"]
            if response.Contains "lighter" || response.Contains "heavier" then
                let (_, ip, rb, mem) =
                    (("", ip, rb, mem), Set.toArray set) 
                    ||> Array.fold (fun (_, ip, rb, mem) inv ->
                        runner ip rb mem ["drop " + inv])
                tester ip rb mem remaining
            else
                sprintf "%A\n" set + response
        | _ -> 
            failwith "no valid found"   

    tester ip rb mem allOptions
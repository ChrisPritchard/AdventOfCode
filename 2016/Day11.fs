(*
--- Day 11: Radioisotope Thermoelectric Generators ---

You come upon a column of four floors that have been entirely sealed off from the rest of the building except for a small dedicated lobby. There are some radiation warnings and a big sign which reads "Radioisotope Testing Facility".

According to the project status board, this facility is currently being used to experiment with Radioisotope Thermoelectric Generators (RTGs, or simply "generators") that are designed to be paired with specially-constructed microchips. Basically, an RTG is a highly radioactive rock that generates electricity through heat.

The experimental RTGs have poor radiation containment, so they're dangerously radioactive. The chips are prototypes and don't have normal radiation shielding, but they do have the ability to generate an electromagnetic radiation shield when powered. Unfortunately, they can only be powered by their corresponding RTG. An RTG powering a microchip is still dangerous to other microchips.

In other words, if a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the chip will be fried. Therefore, it is assumed that you will follow procedure and keep chips connected to their corresponding RTG when they're in the same room, and away from other RTGs otherwise.

These microchips sound very interesting and useful to your current activities, and you'd like to try to retrieve them. The fourth floor of the facility has an assembling machine which can make a self-contained, shielded computer for you to take with you - that is, if you can bring it all of the RTGs and microchips.

Within the radiation-shielded part of the facility (in which it's safe to have these pre-assembly RTGs), there is an elevator that can move between the four floors. Its capacity rating means it can carry at most yourself and two RTGs or microchips in any combination. (They're rigged to some heavy diagnostic equipment - the assembling machine will detach it for you.) As a security measure, the elevator will only function if it contains at least one RTG or microchip. The elevator always stops on each floor to recharge, and this takes long enough that the items within it and the items on that floor can irradiate each other. (You can prevent this if a Microchip and its Generator end up on the same floor in this way, as they can be connected while the elevator is recharging.)

You make some notes of the locations of each component of interest (your puzzle input). Before you don a hazmat suit and start moving things around, you'd like to have an idea of what you need to do.

When you enter the containment area, you and the elevator will start on the first floor.

For example, suppose the isolated area has the following arrangement:

The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.

As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for Generator), the initial state looks like this:

F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 .  HG .  .  .  
F1 E  .  HM .  LM 

Then, to get everything up to the assembling machine on the fourth floor, the following steps could be taken:

    Bring the Hydrogen-compatible Microchip to the second floor, which is safe because it can get power from the Hydrogen Generator:

    F4 .  .  .  .  .  
    F3 .  .  .  LG .  
    F2 E  HG HM .  .  
    F1 .  .  .  .  LM 

    Bring both Hydrogen-related items to the third floor, which is safe because the Hydrogen-compatible microchip is getting power from its generator:

    F4 .  .  .  .  .  
    F3 E  HG HM LG .  
    F2 .  .  .  .  .  
    F1 .  .  .  .  LM 

    Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible Microchip back down with you so you can still use the elevator:

    F4 .  .  .  .  .  
    F3 .  HG .  LG .  
    F2 E  .  HM .  .  
    F1 .  .  .  .  LM 

    At the first floor, grab the Lithium-compatible Microchip, which is safe because Microchips don't affect each other:

    F4 .  .  .  .  .  
    F3 .  HG .  LG .  
    F2 .  .  .  .  .  
    F1 E  .  HM .  LM 

    Bring both Microchips up one floor, where there is nothing to fry them:

    F4 .  .  .  .  .  
    F3 .  HG .  LG .  
    F2 E  .  HM .  LM 
    F1 .  .  .  .  .  

    Bring both Microchips up again to floor three, where they can be temporarily connected to their corresponding generators while the elevator recharges, preventing either of them from being fried:

    F4 .  .  .  .  .  
    F3 E  HG HM LG LM 
    F2 .  .  .  .  .  
    F1 .  .  .  .  .  

    Bring both Microchips to the fourth floor:

    F4 E  .  HM .  LM 
    F3 .  HG .  LG .  
    F2 .  .  .  .  .  
    F1 .  .  .  .  .  

    Leave the Lithium-compatible microchip on the fourth floor, but bring the Hydrogen-compatible one so you can still use the elevator; this is safe because although the Lithium Generator is on the destination floor, you can connect Hydrogen-compatible microchip to the Hydrogen Generator there:

    F4 .  .  .  .  LM 
    F3 E  HG HM LG .  
    F2 .  .  .  .  .  
    F1 .  .  .  .  .  

    Bring both Generators up to the fourth floor, which is safe because you can connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:

    F4 E  HG .  LG LM 
    F3 .  .  HM .  .  
    F2 .  .  .  .  .  
    F1 .  .  .  .  .  

    Bring the Lithium Microchip with you to the third floor so you can use the elevator:

    F4 .  HG .  LG .  
    F3 E  .  HM .  LM 
    F2 .  .  .  .  .  
    F1 .  .  .  .  .  

    Bring both Microchips to the fourth floor:

    F4 E  HG HM LG LM 
    F3 .  .  .  .  .  
    F2 .  .  .  .  .  
    F1 .  .  .  .  .  

In this arrangement, it takes 11 steps to collect all of the objects at the fourth floor for assembly. (Each elevator stop counts as one step, even if nothing is added to or removed from it.)

In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?
*)

module Day11

open Common

let input = 
    [|
        "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
        "The second floor contains a hydrogen generator."
        "The third floor contains a lithium generator."
        "The fourth floor contains nothing relevant."
    |]
//let input = 
//    [|
//        "The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip."
//        "The second floor contains a polonium-compatible microchip and a promethium-compatible microchip."
//        "The third floor contains nothing relevant."
//        "The fourth floor contains nothing relevant."
//    |]

type Kind = Generator of string | Microchip of string

let parseFloor floors (floor, text) = 
    if contains "nothing relevant" text then floors
    else
        let toExclude = [
            "The first floor contains a "
            "The second floor contains a "
            "The third floor contains a "
            "The fourth floor contains a "
            " and a "
            ", a "
            "."
        ]
        let parts = splits toExclude text
        (floors, parts) ||> Array.fold (fun floors p ->
            let segments = split "- " p
            if segments.Length = 3 then
                Map.add (Microchip segments.[0]) (floor + 1) floors
            else
                Map.add (Generator segments.[0]) (floor + 1) floors)

let start = 1, (Map.empty, Array.indexed input) ||> Array.fold parseFloor

let part1 () =
    
    let found = Set.empty.Add start

    let hasFinished (elevator, floors) = 
        elevator = 4 && Map.tryFindKey (fun _ floor -> floor <> 4) floors = None

    let onFloor floor floors =
        floors |> Map.filter (fun _ f -> f = floor) |> Map.toList |> List.map fst 
    
    let rec combinations list =
        match list with
        | [] -> []
        | head::tail ->
            [
                yield [head]
                yield! tail |> List.map (fun k -> [head;k])
                yield! combinations tail
            ]

    let valid cargo dest floors =
        match List.sort cargo with
        | [Generator k1;Microchip k2] when k1 = k2 -> true
        | _ ->
            for

    let options (elevator, floors) =
        let currentContents = onFloor elevator floors
        let pairs = combinations currentContents

        //let adjacent = 
        //    if elevator = 1 then [2] 
        //    elif elevator = 4 then [3] 
        //    else [elevator + 1; elevator - 1]
        //let cargo = adjacent |> List.collect (fun dest -> 
        //    [
        //        yield! pairs |> List.map (fun pair -> dest, pair)
        //        let destContents = onFloor dest floors |> Set.ofList
        //        let chips = chips currentContents |> List.filter (fun (Microchip s) -> destContents.Contains (Generator s))
        //        ()
        //    ])
        0

    let res = options start

    // have a collection of states
    // for each state:
        // if hasFinished state then return steps
        // else derive all possible next states
            // exclude states in found
                // fold through: if in found ignore, else add to found
    // run again with new state set

    // possible next states for a state are:
        // up to two things of any kind that are valid for the adjacent floors
            // things are valid if there is a pair on that floor, or
            // if a chip and generator of the same kind are brought together, or
            // two chips are brought and the destination does not contain rtgs

    // to get pairs and chips for a floor:
        // all pairs on floor (group by type) can go to another floor
        // then any chip can go to dest floor if gen is present or no other unpaired gens are present
        // any gen can go to dest floor if chip is present

    // to get next state:
        // for pair and floornum, state.add gen and state.add chip
        // for chip and floornum, just state.add chip

    0
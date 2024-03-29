let input = System.IO.File.ReadAllLines "input.txt"

let parsed = 
    input |> Array.map (fun line -> 
        let parts = line.Split (" ->,".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
        if parts[0][0] = '%' then
            parts[0][1..], ('%', parts[1..])
        else if parts[0][0] = '&' then
            parts[0][1..], ('&', parts[1..])
        else
            parts[0], ('b', parts[1..]))

let mutable flip_flops = parsed |> Array.choose (fun (k, (t, o)) -> 
    if t <> '%' then None else Some (k, (false, o))) |> Map.ofArray
let mutable conjunctions = 
    parsed |> Array.choose (fun (k, (t, o)) -> 
    if t <> '&' then None else 
        let inputs = parsed |> Array.choose (fun (ok, (_, oo)) -> if Array.contains k oo then Some (ok, false) else None)
        Some (k, (Map.ofArray inputs, o))
    ) |> Map.ofArray
let broadcaster_dests = parsed |> Array.pick (fun (k, (t, o)) -> if t = 'b' then Some o else None)

let rec pulser (pulses: (string * string * bool)[]) low_count high_count pulse_check =
    if Array.isEmpty pulses then
        low_count, high_count
    else
        let mutable new_pulses = Array.empty
        let mutable low_count, high_count = low_count, high_count
        for (from, dest, strength) in pulses do
            if Map.containsKey dest flip_flops then
                let (state, dests) = flip_flops[dest]
                if not strength then
                    let new_state = not state
                    let pulses = dests |> Array.map (fun d -> dest, d, new_state)
                    if new_state then high_count <- high_count + dests.Length else low_count <- low_count + dests.Length
                    flip_flops <- flip_flops.Add (dest, (new_state, dests))
                    new_pulses <- Array.append new_pulses pulses
            else if Map.containsKey dest conjunctions then
                let (states, dests) = conjunctions[dest]
                let new_states = states.Add (from, strength)
                let pulse_strength = not (Map.forall (fun _ b -> b) new_states)
                let pulses = dests |> Array.map (fun d -> dest, d, pulse_strength)
                if pulse_strength then high_count <- high_count + dests.Length else low_count <- low_count + dests.Length
                conjunctions <- conjunctions.Add (dest, (new_states, dests))
                new_pulses <- Array.append new_pulses pulses
        pulse_check pulses
        pulser new_pulses low_count high_count pulse_check

let start_pulses = broadcaster_dests |> Array.map (fun d -> "broadcaster", d, false)
let low, high = 
    [1..1000] |> List.fold (fun (low, high) _ -> 
        let nlow, nhigh = pulser start_pulses (start_pulses.Length + 1) 0 (fun _ -> ())
        low + nlow, high + nhigh) (0, 0)
        
printfn "Part 1: %d" (low * high)

flip_flops <- Map.map (fun _ (_, dests) -> false, dests) flip_flops
conjunctions <- Map.map (fun _ (a, dests) -> Map.map (fun _ _ -> false) a, dests) conjunctions

// rx is fed by kh
// kh is a conjunction fed by hz, pv, xm, qh
// find when all four will feed true at once

let mutable cycles = Map.empty
Seq.initInfinite id |> Seq.find (fun i -> 
    let mutable found = false
    pulser start_pulses (start_pulses.Length + 1) 0 (fun a -> 
        let hits = a |> Array.filter (fun (k, d, s) -> d = "kh" && s) |> Array.map (fun (k, _, _) -> k)
        for k in hits do
            if not (Map.containsKey k cycles) then
                cycles <- cycles.Add (k, i + 1)) |> ignore
    cycles.Count = 4) |> ignore

let rec gcd a b = if b = 0UL then a else gcd b (a % b)
let lcm = cycles |> Seq.map (fun kv -> uint64 kv.Value) |> Seq.reduce (fun a b -> (a * b) / gcd a b)

let part2 = lcm
printfn "Part 2: %d" part2
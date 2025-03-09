let input = System.IO.File.ReadAllLines "input.txt"

let input_folder (wires, circuits) (line: string) =
    if line.Contains ":" then
        let p = line.Split ":"
        (p[0], p[1] = " 1") :: wires, circuits
    else if line.Contains "->" then
        let p =
            line.Split(" ->".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)

        wires, (p[0], p[2], p[1], p[3]) :: circuits
    else
        wires, circuits

let input_wires, input_circuits = Array.fold input_folder ([], []) input

let mutable full_circuit = Map.empty

for i1, i2, op, out in input_circuits do
    full_circuit <-
        Map.add
            out
            (fun () ->

                match op with
                | "AND" -> full_circuit[i1]() && full_circuit[i2]()
                | "OR" -> full_circuit[i1]() || full_circuit[i2]()
                | "XOR" -> full_circuit[i1]() <> full_circuit[i2]()
                | _ -> failwithf "invalid op: %s" op)
            full_circuit

let set_input (key: char) (bits: bool array) =
    for i, b in Array.indexed bits do
        full_circuit <- Map.add (sprintf "%c%02d" key i) (fun () -> b) full_circuit

let x_s =
    input_wires
    |> Seq.filter (fun (k, _) -> k.StartsWith "x")
    |> Seq.sort
    |> Seq.map snd
    |> Seq.toArray

let y_s =
    input_wires
    |> Seq.filter (fun (k, _) -> k.StartsWith "y")
    |> Seq.sort
    |> Seq.map snd
    |> Seq.toArray

set_input 'x' x_s
set_input 'y' y_s

let as_num bits =
    bits
    |> Array.rev
    |> Array.map (fun v -> if v then '1' else '0')
    |> System.String
    |> fun s -> System.Convert.ToUInt64(s, 2)

let out_length =
    input_circuits
    |> List.choose (fun (_, _, _, out) ->
        if out.StartsWith "z" then
            Some(System.Int32.Parse out[1..])
        else
            None)
    |> List.max
    |> (+) 1

let z_s = Array.init out_length (fun i -> full_circuit[sprintf "z%02d" i]())

printfn "Part 1: %d" <| as_num z_s

// following rules defined in the code here: https://www.bytesizego.com/blog/aoc-day24-golang

let invalid =
    input_circuits
    |> Seq.choose (fun (i1, i2, op, out) ->
        if out[0] = 'z' && out <> sprintf "z%02d" (z_s.Length - 1) && op <> "XOR" then
            Some out
        else if
            out[0] <> 'z'
            && i1[0] <> 'x'
            && i2[0] <> 'x'
            && i1[0] <> 'y'
            && i2[0] <> 'y'
            && op = "XOR"
        then
            Some out
        else if
            (i1[0] = 'x' && i2[0] = 'y' || i1[0] = 'y' && i2[0] = 'x')
            && i1 <> "x00"
            && i1 <> "y00"
            && i2 <> "x00"
            && i2 <> "y00"
        then
            if
                op = "XOR"
                && List.tryFind (fun (oi1, oi2, oop, _) -> oop = "XOR" && (oi1 = out || oi2 = out)) input_circuits = None
            then
                Some out
            else if
                op = "AND"
                && List.tryFind (fun (oi1, oi2, oop, _) -> oop = "OR" && (oi1 = out || oi2 = out)) input_circuits = None
            then
                Some out
            else
                None
        else
            None)

printfn "Part 2: %s" <| (invalid |> Seq.sort |> String.concat ",")

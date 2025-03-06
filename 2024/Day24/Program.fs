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

let input_wires, input_circuits =
    Array.fold input_folder ([], []) input
    |> fun (w, c) -> Map.ofList w, Array.ofList c

let rec processor wires (circuits: (string * string * string * string)[]) =
    let next, rem =
        circuits
        |> Array.partition (fun (i1, i2, _, _) -> Map.containsKey i1 wires && Map.containsKey i2 wires)

    if next.Length = 0 then
        wires
    else

        let circuit_runner (new_wires: Map<string, bool>) valid_circuit =
            let i1, i2, op, out = valid_circuit

            let v =
                match op with
                | "AND" -> new_wires[i1] && new_wires[i2]
                | "OR" -> new_wires[i1] || new_wires[i2]
                | "XOR" -> new_wires[i1] <> new_wires[i2]
                | s -> failwithf "invalid op: %s" s

            Map.add out v new_wires

        let new_wires = Array.fold circuit_runner wires next
        processor new_wires rem

let final_wires = processor input_wires input_circuits

let out =
    final_wires
    |> Map.toArray
    |> Array.filter (fun (k, _) -> k[0] = 'z')
    |> Array.sortBy fst
    |> Array.map (snd >> fun v -> if v then '1' else '0')
    |> Array.rev
    |> System.String

printfn "Part 1: %d" <| System.Convert.ToUInt64(out, 2)

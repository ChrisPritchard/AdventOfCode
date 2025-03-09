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
let mutable gate_log = fun out -> ()
let mutable stack_depth = 0

let max_depth_of_stack = 100

for i1, i2, op, out in input_circuits do
    full_circuit <-
        Map.add
            out
            (fun () ->
                gate_log out
                stack_depth <- stack_depth + 1

                if stack_depth > max_depth_of_stack then
                    failwith "loop detected"

                match op with
                | "AND" -> full_circuit[i1]() && full_circuit[i2]()
                | "OR" -> full_circuit[i1]() || full_circuit[i2]()
                | "XOR" -> full_circuit[i1]() <> full_circuit[i2]()
                | _ -> failwithf "invalid op: %s" op)
            full_circuit

let get_output i =
    stack_depth <- 0
    full_circuit[sprintf "z%02d" i]()

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

let out_length = x_s.Length + 1

let z_s = Array.init out_length get_output

let as_num =
    z_s
    |> Array.rev
    |> Array.map (fun v -> if v then '1' else '0')
    |> System.String
    |> fun s -> System.Convert.ToUInt64(s, 2)

printfn "Part 1: %d" as_num

// set_input 'y' (Array.init y_s.Length (fun i -> if i = 0 then true else false))
// set_input 'x' (Array.init x_s.Length (fun i -> false))

// let test_out = Array.init out_length get_output
// printfn "%A" test_out

let mutable swapped: string list = []

let check up_to =
    let new_z = Array.init out_length get_output

    let rec adder i carry =
        let expected = x_s[i] <> y_s[i] <> carry
        let new_carry = x_s[i] && y_s[i] || x_s[i] && carry || y_s[i] && carry

        if new_z[i] <> expected then false
        else if i = up_to then true
        else adder (i + 1) new_carry

    adder 0 false

let mutable gates = []

gate_log <-
    fun o ->
        if not (o.StartsWith "z") && not (List.contains o swapped) then
            gates <- o :: gates

for i in 0 .. out_length - 2 do
    gates <- []

    if not (check i) then
        printfn "failed at bit %i" i

        let possible_pairs =
            [ for i in 0 .. gates.Length - 2 do
                  for j in i + 1 .. gates.Length - 1 do
                      yield gates[i], gates[j] ]

        let rec find_swap =
            function
            | [] -> failwith "couldn't find correction"
            | (a, b) :: rem ->
                let a_f = full_circuit[a]
                let b_f = full_circuit[b]
                full_circuit <- Map.add a b_f full_circuit
                full_circuit <- Map.add b a_f full_circuit

                let result =
                    try
                        check i
                    with _ ->
                        false

                if result then
                    swapped <- a :: b :: swapped
                    printfn "fixed with swap %s <-> %s" a b
                    ()
                else
                    full_circuit <- Map.add a a_f full_circuit
                    full_circuit <- Map.add b b_f full_circuit
                    find_swap rem

        find_swap possible_pairs

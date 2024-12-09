let input = System.IO.File.ReadAllLines "input.txt"

let values = input[0] |> Seq.toArray |> Array.map (fun c -> int c - int '0')

let memory = Array.create (Array.sum values) -1

let rec layout i j =
    if i = values.Length then
        ()
    else
        if i % 2 = 0 then
            for k in [ j .. j + values[i] - 1 ] do
                memory[k] <- i / 2

        layout (i + 1) (j + values[i])

let rec compact i j =
    if i = memory.Length || i > j then
        ()
    else if memory[i] <> -1 then
        compact (i + 1) j
    else if memory[j] = -1 then
        compact i (j - 1)
    else
        memory[i] <- memory[j]
        memory[j] <- -1
        compact (i + 1) (j - 1)

layout 0 0
compact 0 (memory.Length - 1)

let checksum =
    memory
    |> Seq.indexed
    |> Seq.sumBy (fun (i, data_id) -> if data_id = -1 then 0L else int64 (i * data_id))

printfn "Part 1: %d" checksum

for i in [ 0 .. memory.Length - 1 ] do
    memory[i] <- -1

layout 0 0

let spaces =
    values
    |> Array.indexed
    |> Array.filter (fun (i, _) -> i % 2 = 1)
    |> Array.map (fun (i, space_available) -> Array.sum values[0 .. i - 1], space_available)

let data =
    values
    |> Array.indexed
    |> Array.filter (fun (i, _) -> i % 2 = 0)
    |> Array.map (fun (i, data_size) -> i / 2, Array.sum values[0 .. i - 1], data_size)

for i in [ data.Length - 1 .. (-1) .. 0 ] do
    let data_id, data_index, data_size = data[i]

    let found =
        spaces
        |> Array.tryFindIndex (fun (space_index, space) -> space_index < data_index && space >= data_size)

    match found with
    | None -> ()
    | Some space_index ->
        let i, space = spaces[space_index]

        for j in [ i .. i + data_size - 1 ] do
            memory[j] <- data_id

        for j in [ data_index .. data_index + data_size - 1 ] do
            memory[j] <- -1

        spaces[space_index] <- (i + data_size, space - data_size)

let new_checksum =
    memory
    |> Seq.indexed
    |> Seq.sumBy (fun (i, data_id) -> if data_id = -1 then 0L else int64 (i * data_id))

printfn "Part 2: %d" new_checksum

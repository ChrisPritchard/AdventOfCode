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
    |> Seq.sumBy (fun (i, v) -> if v = -1 then 0L else int64 (i * v))

printfn "Part 1: %d" checksum

let rec compressor sum head forward_index back_index space_available =
    printfn "sum: %d head: %d forward: %d backward: %d space: %d" sum head forward_index back_index space_available

    if forward_index = values.Length || forward_index > back_index then
        sum
    else if forward_index % 2 = 0 then
        printfn "data %d size %d at position %d" (forward_index / 2) values[forward_index] head

        let new_sum =
            sum
            + int64 (
                [ 0 .. values[forward_index] - 1 ]
                |> Seq.sumBy (fun i -> (head + i) * (forward_index / 2))
            )

        let new_head = head + values[forward_index]
        compressor new_sum new_head (forward_index + 1) back_index (values[forward_index + 1])
    else if values[back_index] > space_available then
        printfn "not enough space for %d size %d at %d" (back_index / 2) values[back_index] head
        compressor sum (head + space_available) (forward_index + 1) back_index 0
    else
        printfn "moved data %d size %d to position %d" (back_index / 2) values[back_index] head

        let new_sum =
            sum
            + int64 (
                [ 0 .. values[back_index] - 1 ]
                |> Seq.sumBy (fun i -> (head + i) * (back_index / 2))
            )

        let new_space_available = space_available - values[back_index]
        let new_head = head + values[back_index]
        compressor sum new_head forward_index (back_index - 1) new_space_available

let new_checksum = compressor 0 0 0 (values.Length - 1) 0

printfn "Part 2: %d" new_checksum

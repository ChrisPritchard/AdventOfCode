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

let input = Input.value

let segments = input.Split [|','|]

let hash (s: string) = 
    let mutable total = 0
    for c in s do
        total <- total + int c
        total <- total * 17
        total <- total % 256
    total

let part1 = segments |> Array.sumBy hash
printfn "Part 1: %d" part1

// go through each segement
// split on = or -
// hash first segment to find box
// if len = 2 then add/replace lens in box with first segment as label and second part as strength
// if len = 1 then remove lens with label
// each box needs a linked list or a collection thats replaced

let lense_boxes: (string * int)[][] = Array.create 256 Array.empty
for segment in segments do
    let parts = segment.Split([|'=';'-'|], System.StringSplitOptions.RemoveEmptyEntries)
    let box = hash parts[0]
    if parts.Length = 2 then
        lense_boxes[box] <-
            match Array.tryFindIndex (fst >> (=) parts[0]) lense_boxes[box] with
            | Some i -> Array.updateAt i (parts[0], int parts[1]) lense_boxes[box]
            | None -> Array.append lense_boxes[box] [|parts[0], int parts[1]|]
    else
        lense_boxes[box] <-
            match Array.tryFindIndex (fst >> (=) parts[0]) lense_boxes[box] with
            | Some i -> Array.removeAt i lense_boxes[box]
            | None -> lense_boxes[box]

let total = lense_boxes |> Array.indexed |> Array.sumBy (fun (i, box) -> box |> Array.indexed |> Array.sumBy (fun (j, (_, v)) -> (j+1) * v * (i+1)))
printfn "Part 2: %d" total
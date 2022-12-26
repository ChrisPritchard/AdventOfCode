module Day17

open Common

// shapes appear two from left, three above top x or floor

let shapes = [|
    [| // horizontal bar
        0b00111100uy
    |]
    [| // cross / plus symbol
        0b00010000uy
        0b00111000uy
        0b00010000uy
    |]
    [| // reverse L
        0b00001000uy
        0b00001000uy
        0b00111000uy
    |]
    [| // vertical bar
        0b00100000uy
        0b00100000uy
        0b00100000uy
        0b00100000uy
    |]
    [| // box / square
        0b00110000uy
        0b00110000uy
    |]
|]

// useful for debugging
let renderStack stack = 
    for r in Array.rev stack do
        let s = 
            [0..6] |> List.map (fun n -> if (0b10000000uy >>> n) &&& r = (0b10000000uy >>> n) then '#' else '.') |> asString
        printfn "|%s|" s

let overlaps shape (stack: byte[]) = 
    Array.exists (fun (row, bits) -> row < stack.Length && (bits &&& stack[row]) <> 0uy) shape

let shunt jet (stack: byte[]) shape =
    if jet = '<' then
        if Array.exists (fun (_, bits) -> 0b10000000uy &&& bits <> 0uy) shape then 
            shape // no move because wall is blocking
        else
            let newShape = Array.map (fun (row, bits) -> row, (bits <<< 1)) shape
            if overlaps newShape stack then shape else newShape
    else if jet = '>' then
        if Array.exists (fun (_, bits) -> 0b00000010uy &&& bits <> 0uy) shape then 
            shape // no move because wall is blocking
        else
            let newShape = Array.map (fun (row, bits) -> row, (bits >>> 1)) shape
            if overlaps newShape stack then shape else newShape
    else
        failwithf "unexpected character: '%A'" jet

let drop shape = Array.map (fun (row, bits) -> row - 1, bits) shape

let nextShape currentShapeIndex topBlock = 
    let nextShapeIndex = if currentShapeIndex = shapes.Length - 1 then 0 else currentShapeIndex + 1
    let nextShape = shapes[nextShapeIndex]
    let shapeTop = nextShape.Length + 2 + topBlock
    let nextShape = nextShape |> Array.indexed |> Array.map (fun (row, bits) -> (shapeTop - row), bits)
    nextShape, nextShapeIndex

let addShapeToStack shape (stack: byte[]) = 
    let shapeRows = Map.ofArray shape
    let newRows = shape |> Array.filter (fun (i, _) -> i >= stack.Length) |> Array.sortBy fst |> Array.map snd
    stack
    |> Array.mapi (fun row bits ->
        if Map.containsKey row shapeRows then bits ||| shapeRows[row] else bits)
    |> fun a -> Array.append a newRows

let part1 () =
    let originalMoves = readEmbedded "day17" |> Array.head |> Seq.toList
    let maxShapes = 2022

    let rec tick shape shapeIndex shapeCount (stack: byte[]) moves =
        match moves with
        | [] -> tick shape shapeIndex shapeCount stack originalMoves // loop around moves
        | jet::rem ->
            let shape = shunt jet stack shape           
            let newShape = drop shape
            if not (overlaps newShape stack) then 
                tick newShape shapeIndex shapeCount stack rem
            else
                let newStack = addShapeToStack shape stack
                if (shapeCount + 1) = maxShapes then 
                    // renderStack newStack
                    newStack.Length - 1
                else
                    let nextShape, nextShapeIndex = nextShape shapeIndex newStack.Length
                    // printfn "%A" nextShape
                    tick nextShape nextShapeIndex (shapeCount + 1) newStack rem

    let shapeTop = shapes[0].Length + 3
    let firstShape = shapes[0] |> Array.indexed |> Array.map (fun (row, bits) -> (shapeTop - row), bits)
    tick firstShape 0 0 [| 0b11111111uy |] originalMoves

let part2 () =
    // let target = 1000000000000L // too large to run

    0

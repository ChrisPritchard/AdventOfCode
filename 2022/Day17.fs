module Day17

open Common

// shapes appear two from left, three above top x or floor

let shapes = [|
    [| // horizontal bar
        0b0011110uy
    |]
    [| // cross / plus symbol
        0b0001000uy
        0b0011100uy
        0b0001000uy
    |]
    [| // reverse L
        0b0000100uy
        0b0000100uy
        0b0011100uy
    |]
    [| // vertical bar
        0b0010000uy
        0b0010000uy
        0b0010000uy
        0b0010000uy
    |]
    [| // box / square
        0b0011000uy
        0b0011000uy
    |]
|]

// useful for debugging
let renderStack stack = 
    let keys = Map.keys stack
    for r in [Seq.max keys..(-1)..0] do
        let s = 
            if not (Map.containsKey r stack) then "......." 
            else [0..6] |> List.map (fun n -> if (0b10000000uy >>> n) &&& stack[r] = (0b10000000uy >>> n) then '#' else '.') |> asString
        printfn "|%s|" s

let part1 () =

    let originalMoves = readEmbedded "day17" |> Array.head |> Seq.collect (fun a -> [a;'v']) |> Seq.toList
    let max = 2022

    let overlaps shape stack = 
        Array.exists (fun (row, bits) -> Map.containsKey row stack && (bits &&& stack[row]) <> 0uy) shape

    let rec tick shape shapeIndex shapeCount stack moves =
        match moves with
        | [] -> tick shape shapeIndex shapeCount stack originalMoves // loop around moves
        | '<'::rem ->
            if Array.exists (fun (_, bits) -> 0b10000000uy &&& bits <> 0uy) shape then 
                tick shape shapeIndex shapeCount stack rem // no move because wall is blocking
            else
                let newShape = Array.map (fun (row, bits) -> row, (bits <<< 1)) shape
                let nextShape = if overlaps newShape stack then shape else newShape
                tick nextShape shapeIndex shapeCount stack rem
        | '>'::rem ->
            if Array.exists (fun (_, bits) -> 0b00000010uy &&& bits <> 0uy) shape then 
                tick shape shapeIndex shapeCount stack rem // no move because wall is blocking
            else
                let newShape = Array.map (fun (row, bits) -> row, (bits >>> 1)) shape
                let nextShape = if overlaps newShape stack then shape else newShape
                tick nextShape shapeIndex shapeCount stack rem
        | _::rem -> // down, or 'v'
            let newShape = Array.map (fun (row, bits) -> row - 1, bits) shape
            let blocked = overlaps newShape stack
            if not blocked then 
                tick newShape shapeIndex shapeCount stack rem
            else
                let newStack = 
                    (stack, shape) 
                    ||> Array.fold (fun stack (row, bits) -> 
                        let newBits = if Map.containsKey row stack then stack[row] ||| bits else bits
                        Map.add row newBits stack)
                
                let stackTop = Map.keys newStack |> Seq.max
                if shapeCount = max then 
                    // renderStack newStack
                    Map.keys newStack |> Seq.max // final result
                else
                    let nextShapeIndex = if shapeIndex = shapes.Length - 1 then 0 else shapeIndex + 1
                    let nextShape = shapes[nextShapeIndex]
                    let shapeTop = nextShape.Length + 5 + stackTop
                    let nextShape = nextShape |> Array.indexed |> Array.map (fun (row, bits) -> (shapeTop - row), bits)
                    tick nextShape nextShapeIndex (shapeCount + 1) newStack rem

    let shapeTop = shapes[0].Length + 2 
    let firstShape = shapes[0] |> Array.indexed |> Array.map (fun (row, bits) -> (shapeTop - row), bits)
    tick firstShape 0 0 (Map [ -1, 0b11111111uy ]) originalMoves

let part2 () =
    // let max = 1000000000000L

    // need a better approach
    // treat each line as a byte?
    // when moving down, if line is 0 then free
    // if line is not zero, then and the two numbers and xor them - if these are the same then its free
    // a shape is made of multiple numbers, based on height
    // when jetting left or right, bit shift the number 

    0

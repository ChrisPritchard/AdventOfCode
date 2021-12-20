module Day20

open Common
open System

let processed = readEmbedded "day20"

let init () =
    processed |> Array.length |> ignore

let algo = processed[0].ToCharArray()

let part1 () =
    
    let pic = 
        let start = processed[1..]
        let m = Array2D.create (start.Length + 2) (start[0].Length + 2) '.'
        for y in [0..start.Length - 1] do
            for x in [0..start[0].Length - 1] do
                m[y + 1, x + 1] <- start[y][x]
        m

    let tryGet defaultChar y x m =
        try
            Array2D.get m y x
        with
        | _ ->
            defaultChar

    let index defaultChar y x m =
        [|x-1, y-1; x, y-1; x+1, y-1; x-1, y; x,y; x+1, y; x-1, y+1; x, y+1; x+1, y+1|]
        |> Array.map (fun (x, y) -> if tryGet defaultChar y x m = '#' then '1' else '0')
        |> asString
        |> fun s -> Convert.ToInt32(s, 2)
        
    let enhance defaultChar m =
        Array2D.init (Array2D.length1 m + 2) (Array2D.length2 m + 2) (fun y x -> algo[index defaultChar (y - 1) (x - 1) m])
  
    let altChar = if algo[0] = '#' then '#' else '.'
    let result = [1..2] |> List.fold (fun m i -> enhance (if i % 2 = 1 then '.' else altChar) m) pic
    let mutable count = 0
    Array2D.iter (fun v -> if v = '#' then count <- count + 1) result
    count
    
    // for debugging
    // let mutable res = "\n"
    // for y in [0..Array2D.length1 result - 1] do
    //     for x in [0..Array2D.length2 result - 1] do
    //         res <- res + string result[y,x]  
    //     res <- res + "\n"
    // res

let part2 () =
    
    let pic = 
        let start = processed[1..]
        let m = Array2D.create (start.Length + 2) (start[0].Length + 2) '.'
        for y in [0..start.Length - 1] do
            for x in [0..start[0].Length - 1] do
                m[y + 1, x + 1] <- start[y][x]
        m

    let tryGet defaultChar y x m =
        try
            Array2D.get m y x
        with
        | _ ->
            defaultChar

    let index defaultChar y x m =
        [|x-1, y-1; x, y-1; x+1, y-1; x-1, y; x,y; x+1, y; x-1, y+1; x, y+1; x+1, y+1|]
        |> Array.map (fun (x, y) -> if tryGet defaultChar y x m = '#' then '1' else '0')
        |> asString
        |> fun s -> Convert.ToInt32(s, 2)
        
    let enhance defaultChar m =
        Array2D.init (Array2D.length1 m + 2) (Array2D.length2 m + 2) (fun y x -> algo[index defaultChar (y - 1) (x - 1) m])
  
    let altChar = if algo[0] = '#' then '#' else '.'
    let result = [1..50] |> List.fold (fun m i -> enhance (if i % 2 = 1 then '.' else altChar) m) pic
    let mutable count = 0
    Array2D.iter (fun v -> if v = '#' then count <- count + 1) result
    count
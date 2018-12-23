open System.IO

type Tile = Plains | Forest | Lumbermill

let newTile x y map =
    let maxX, maxY = Array2D.length1 map, Array2D.length2 map
    let neighbours =
        [-1..1] |> List.collect (fun dx -> [-1, 1] |> List.map (fun dy -> x + dx, y + dy))
        |> List.filter (fun (nx, ny) -> 
            (nx, ny) <> (x, y) && nx >= 0 && ny >= 0 && nx < maxX && ny < maxY)
        |> List.map (fun (nx, ny) -> map.[nx, ny])
        |> List.groupBy id
        |> List.map (fun o -> fst o, snd o |> List.length)
        |> Map.ofList
    match map.[x, y] with
    | Plains -> if neighbours.ContainsKey Forest && neighbours.[Forest] >= 3 then Forest else Plains
    | Forest -> if neighbours.ContainsKey Lumbermill && neighbours.[Lumbermill] >= 3 then Lumbermill else Forest
    | Lumbermill -> if neighbours.ContainsKey Lumbermill && neighbours.ContainsKey Forest then Lumbermill else Plains

let advance map =
    let next = Array2D.create (Array2D.length1 map) (Array2D.length2 map) Plains
    for x = 0 to Array2D.length1 map - 1 do
        for y = 0 to Array2D.length2 map - 1 do
            next.[x, y] <- newTile x y map
    next

let parseInput (lines: string []) =
    let map = Array2D.create lines.[0].Length lines.Length Plains
    lines 
    |> Array.mapi (fun y line -> line |> Seq.mapi (fun x c -> x, y, c))
    |> Seq.collect id 
    |> Seq.iter (fun (x, y, c) -> 
        map.[x, y] <-
            match c with
            | '|' -> Forest
            | '#' -> Lumbermill
            | _ -> Plains)
    map

let renderOutput map =
    [0..Array2D.length2 map - 1] |> List.map (fun y ->
        [0..Array2D.length1 map - 1] |> List.map (fun x -> 
            match map.[x, y] with Plains -> "." | Forest -> "|" | Lumbermill -> "#") |> String.concat "")

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines("sample.txt")
    let map = parseInput input
    renderOutput map |> List.iter (printfn "%s")
    
    0

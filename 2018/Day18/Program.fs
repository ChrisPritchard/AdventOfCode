open System.IO

type Tile = Plains | Forest | Lumbermill

let newTile x y map =
    let maxX, maxY = Array2D.length1 map, Array2D.length2 map
    let neighbours =
        [-1..1] |> List.collect (fun dx -> [-1..1] |> List.map (fun dy -> x + dx, y + dy))
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

let resourceValue map =
    let result =
        [0..Array2D.length2 map - 1] |> List.collect (fun y ->
        [0..Array2D.length1 map - 1] |> List.map (fun x -> map.[x, y]))
        |> List.groupBy id
        |> List.map (fun (key, list) -> key, List.length list)
        |> Map.ofList
    if not <| result.ContainsKey Lumbermill || not <| result.ContainsKey Forest then 0
    else result.[Lumbermill] * result.[Forest]

let resourceValueAfter1000 n after10 =
    let scores = 
        [11..1000] |> List.mapFold (fun current i -> 
        let next = advance current
        let score = resourceValue next
        (i, score), next) after10
        |> fst |> Map.ofList
    let prev = 
        [999..-1..11] |> List.fold (fun found i ->
            match found with
            | 0 -> if scores.[i] = scores.[1000] then i else 0
            | _ -> found) 0
    let range = 1000 - prev    
    let index = 1000 - (range - ((n - prev) % range))
    scores.[index]

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines("input.txt")
    let startMap = parseInput input
    let after10 = [1..10] |> List.fold (fun current _ -> advance current) startMap
    
    printfn "part 1: %i" <| resourceValue after10
    printfn "part 2: %i" <| resourceValueAfter1000 1000000000 after10    

    0

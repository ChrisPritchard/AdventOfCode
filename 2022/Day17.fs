module Day17

open Common

// shapes appear two from left, three above top x or floor

type Shape =
    | Across
    | Cross
    | ReverseL
    | Vertical
    | Box

let nextShape =
    function
    | Across -> Cross
    | Cross -> ReverseL
    | ReverseL -> Vertical
    | Vertical -> Box
    | Box -> Across

let spawn topY =
    let y = topY + 4
    function
    | Across -> set [2,y; 3,y; 4,y; 5,y]
    | Cross -> set [3,y; 2,y+1; 3,y+1; 4,y+1; 3,y+2]
    | ReverseL -> set [2,y; 3,y; 4,y; 4,y+1; 4,y+2]
    | Vertical -> set [2,y; 2,y+1; 2,y+2; 2,y+3]
    | Box -> set [2,y; 3,y; 2,y+1; 3,y+1;]

let move mv = 
    match mv with
    | '<' -> Set.map (fun (x, y) -> x - 1, y)
    | '>' -> Set.map (fun (x, y) -> x + 1, y)
    | _ -> Set.map (fun (x, y) -> x, y - 1)

let notWall = Set.forall (fun (x, _) -> x >= 0 && x < 7)
let notFloor = Set.forall (fun (_, y) -> y > 0)

let top = Seq.maxBy snd >> snd

let part1 () =

    let moves = readEmbedded "day17" |> Array.head |> Seq.toList

    let max = 2022
    let rec tick count blocked shapeType shape =
        function
        | [] -> tick count blocked shapeType shape moves
        | next::rem ->
            let shape = 
                let newShape = move next shape
                if Set.isEmpty (Set.intersect newShape blocked)  && notWall newShape then newShape else shape
            let drop = move 'V' shape
            if Set.isEmpty (Set.intersect drop blocked)  && notFloor drop 
            then tick count blocked shapeType drop rem
            else if count = max then top (Set.union shape blocked)
            else
                let blocked = Set.union shape blocked
                let shapeType = nextShape shapeType
                let shape = spawn (top blocked) shapeType
                tick (count + 1) blocked shapeType shape rem

    tick 1 Set.empty Across (spawn 0 Across) []

let part2 () =
    0
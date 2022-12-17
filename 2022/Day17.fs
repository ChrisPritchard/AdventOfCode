module Day17

open Common

// shapes appear two from left, three above top x or floor

type Shape =
    | Across
    | Cross
    | ReverseL
    | Vertical
    | Box

let spawn topY =
    let y = topY - 4
    function
    | Across -> set [2,y; 3,y; 4,y; 5,y]
    | Cross -> set [3,y; 2,y-1; 3,y-1; 4,y-1; 3,y-2]
    | ReverseL -> set [2,y; 3,y; 4,y; 4,y-1; 4,y-2; 4,y-3]
    | Vertical -> set [2,y; 2,y-1; 2,y-2; 2,y-3]
    | Box -> set [2,y; 3,y; 2,y-1; 3,y-1;]

let move dx dy shape =
    Set.map (fun (x, y) -> x + dx, y + dy) shape

let part1 () =
    0

let part2 () =
    0
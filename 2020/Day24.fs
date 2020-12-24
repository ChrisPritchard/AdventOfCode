module Day24

open System.IO

let input = File.ReadAllLines "./inputs/day24.txt"

let processed () = 
    let rec readLine acc rem =
        match rem with
        | 'e'::rem -> readLine ((1,-1,0)::acc) rem
        | 's'::'e'::rem -> readLine ((0,-1,1)::acc) rem
        | 's'::'w'::rem -> readLine ((-1,0,1)::acc) rem
        | 'w'::rem -> readLine ((-1,1,0)::acc) rem
        | 'n'::'w'::rem -> readLine ((0,1,-1)::acc) rem
        | 'n'::'e'::rem -> readLine ((1,0,-1)::acc) rem
        | _ -> ((0, 0, 0), List.rev acc) ||> List.fold (fun (x, y, z) (dx, dy, dz) -> x + dx, y + dy, z + dz) 
    input
    |> Array.map (Seq.toList >> readLine [])

let part1 () = 
    (Set.empty, processed ())
    ||> Array.fold (fun acc tile -> if Set.contains tile acc then Set.remove tile acc else Set.add tile acc)
    |> Set.count

let part2 () =
    0
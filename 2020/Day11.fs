module Day11

open System.IO

let input = File.ReadAllLines "./inputs/day11.txt"

let processed () =
    input 
    |> Array.mapi (fun y line -> 
        line |> Seq.toArray |> Array.mapi (fun x c -> (y, x), c))
    |> Array.collect id
    |> Map.ofArray

let cardinals = 
    [|
        -1,-1;-1,0;-1,1;
        0,-1;0,1;
        1,-1;1,0;1,1;
    |]

let part1 () =
    let state (y, x) c a =
        let occupied = 
            cardinals 
            |> Array.filter (fun (dy, dx) -> 
                let p = (y + dy, x + dx)
                Map.containsKey p a && a.[p] = 'L') 
            |> Array.length
        match c, occupied with
        | 'L', 0 -> '#'
        | '#', n when n >= 4 -> 'L'
        | c, _ -> c

    let rec iter a =
        let n = Map.map (fun p v -> state p v a) a
        if n = a then 
            Map.filter (fun _ v -> v = '#') n
            |> Map.count
        else
            iter n

    iter (processed ())

let part2 () =
    0
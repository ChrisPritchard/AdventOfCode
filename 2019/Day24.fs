module Day24

open System.IO

let input = File.ReadAllLines "./inputs/day24.txt" |> Array.map (fun line -> line.ToCharArray ())

let part1 () =

    let valid (map: char [][]) (x, y) = x >= 0 && y >= 0 && y < map.Length && x < map.[y].Length
    
    let adjacent map (x, y) =
        [|
            x, y - 1
            x, y + 1
            x + 1, y
            x - 1, y
        |] 
        |> Array.filter (valid map)
        |> Array.map (fun (x, y) -> (x, y), map.[y].[x])
    
    let bugs map pos =
        adjacent map pos |> Array.filter (snd >> (=) '#') |> Array.length
    
    let biorating map =
        Array.collect id map
        |> Array.indexed 
        |> Array.filter (snd >> (=) '#') 
        |> Array.sumBy (fun (index, _) -> pown 2L index)

    let rec minute map visited =
        let next = 
            map
            |> Array.mapi (fun y line ->
                line |> Array.mapi (fun x cell ->
                    let bugs = bugs map (x, y)
                    if cell = '#' && bugs = 1 then '#'
                    elif cell = '.' && (bugs = 1 || bugs = 2) then '#'
                    else '.'))
        // printwait next
        if Set.contains next visited then biorating next
        else
            minute next (Set.add next visited)

    minute input (Set.empty.Add input) |> string

let part2 () =

    0
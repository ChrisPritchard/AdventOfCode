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

    let adjacent (ox, oy, l) =
        let borders (x, y) = 
            if x = -1 then
                [|1, 2, l-1|]
            elif x = 5 then
                [|3, 2, l-1|]
            elif y = -1 then
                [|2, 1, l-1|]
            elif y = 5 then 
                [|2, 3, l-1|]
            elif (x, y) <> (2, 2) then 
                [|x, y, l|]
            else
                [|
                    (1, 2), [| for y = 0 to 4 do yield 0, y, l + 1 |]
                    (3, 2), [| for y = 0 to 4 do yield 4, y, l + 1 |]
                    (2, 1), [| for x = 0 to 4 do yield x, 0, l + 1 |]
                    (2, 3), [| for x = 0 to 4 do yield x, 4, l + 1 |]
                |] |> Map.ofArray |> Map.find (ox, oy)
        [| 
            ox - 1, oy
            ox + 1, oy
            ox, oy - 1
            ox, oy + 1
        |] |> Array.collect borders

    let bugs map (x, y, l) =
        let adjacent = adjacent (x, y, l)
        ((0, map), adjacent) 
        ||> Array.fold (fun (cnt, map) point ->
            if Map.containsKey point map && map.[point] = '#' 
            then cnt + 1, map
            else
                let (x, y, _) = point
                if (x, y) = (2, 2) || x < 0 || x > 4 || y < 0 || y > 4 then System.Diagnostics.Debugger.Break ()
                cnt, Map.add point '.' map)

    let minute map =
        let expandedMap = 
            (map, Map.toArray map) 
            ||> Array.fold (fun map (point, _) ->
                bugs map point |> snd)
            
        let changes =
            ([], Map.toArray expandedMap) 
            ||> Array.fold (fun changes (point, state) ->
                let bugs, _ = bugs expandedMap point 
                if state = '#' then 
                    if bugs = 1 then
                        changes // no change
                    else
                        (point, '.')::changes
                else 
                    if bugs = 1 || bugs = 2 then
                        (point, '#')::changes
                    else
                        changes)

        (expandedMap, changes) 
        ||> List.fold (fun map (point, state) -> Map.add point state map)
    
    let start scan = 
        input 
        |> Array.indexed 
        |> Array.collect (fun (y, line) ->
            line |> Array.indexed |> Array.map (fun (x, cell) -> (x, y, 0), cell))
        |> Map.ofArray
        |> Map.remove (2, 2, 0)

    //let print map =
    //    System.Console.Clear ()
    //    System.Console.CursorTop <- 0
    //    System.Console.CursorLeft <- 0
    //    Map.toArray map
    //    |> Array.groupBy (fun ((_,_,l), _) -> l)
    //    |> Array.sortBy fst
    //    |> Array.iter (fun (level, tiles) ->
    //        printfn "Level %i" level
    //        let levelMap = tiles |> Map.ofArray
    //        for y = 0 to 4 do
    //            for x = 0 to 4 do
    //                if Map.containsKey (x, y, level) levelMap then
    //                    printf "%c" levelMap.[x, y, level]
    //                else
    //                    printf "."
    //            printfn ""
    //        printfn "")
    //    System.Console.ReadKey true
    
    let rec totalAfter minutes map =
        if minutes = 0 then 
            //print map |> ignore
            map |> Map.toArray |> Array.filter (snd >> (=) '#') |> Array.length
        else
            //print map |> ignore
            let next = minute map
            totalAfter (minutes - 1) next

    //let test = [|
    //    "....#".ToCharArray ()
    //    "#..#.".ToCharArray ()
    //    "#.?##".ToCharArray ()
    //    "..#..".ToCharArray ()
    //    "#....".ToCharArray ()
    //    |]

    let minutes, start = 200, start input
    //let minutes, start = 10, start test
    //print start |> ignore
    totalAfter minutes start
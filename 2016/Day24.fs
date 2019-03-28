(*
--- Day 24: Air Duct Spelunking ---

You've finally met your match; the doors that provide access to the roof are locked tight, and all of the controls and related electronics are inaccessible. You simply can't reach them.

The robot that cleans the air ducts, however, can.

It's not a very fast little robot, but you reconfigure it to be able to interface with some of the exposed wires that have been routed through the HVAC system. If you can direct it to each of those locations, you should be able to bypass the security controls.

You extract the duct layout for this area from some blueprints you acquired and create a map with the relevant locations marked (your puzzle input). 0 is your current location, from which the cleaning robot embarks; the other numbers are (in no particular order) the locations the robot needs to visit at least once each. Walls are marked as #, and open passages are marked as .. Numbers behave like open passages.

For example, suppose you have a map like the following:

###########
#0.1.....2#
#.#######.#
#4.......3#
###########

To reach all of the points of interest as quickly as possible, you would have the robot take the following path:

    0 to 4 (2 steps)
    4 to 1 (4 steps; it can't move diagonally)
    1 to 2 (6 steps)
    2 to 3 (2 steps)

Since the robot isn't very fast, you need to find it the shortest route. This path is the fewest steps (in the above example, a total of 14) required to start at 0 and then visit every other location at least once.

Given your actual map, and starting from location 0, what is the fewest number of steps required to visit every non-0 number marked on the map at least once?
*)

(*
--- Part Two ---

Of course, if you leave the cleaning robot somewhere weird, someone is bound to notice.

What is the fewest number of steps required to start at 0, visit every non-0 number marked on the map at least once, and then return to 0?
*)

module Day24

let input = System.IO.File.ReadAllLines "Day24-input.txt"
//let input = 
    //[|
    //    "###########"
    //    "#0.1.....2#"
    //    "#.#######.#"
    //    "#4.......3#"
    //    "###########"
    //|]

let ductSystem, start, numbers = 
    let a = Array2D.create input.[0].Length input.Length '#'
    let mutable numbers = []
    for x = 0 to input.[0].Length - 1 do
        for y = 0 to input.Length - 1 do
            let c = input.[y].[x]
            a.[x, y] <- c
            if System.Char.IsDigit c then 
                numbers <- (c, (x, y))::numbers
    a, List.find (fst >> (=) '0') numbers |> snd, List.filter (fst >> (<>) '0') numbers |> List.map snd

let bfs start goal =
    let rec searcher edges visited steps = 
        let adjacent (x, y) = [-1,0;1,0;0,-1;0,1] |> List.map (fun (dx, dy) -> dx + x, dy + y)
        let newEdges, newVisited =
            (([], visited), edges)
            ||> List.fold (fun (acc, visited) edge ->
                ((acc, visited), adjacent edge)
                ||> List.fold (fun (acc, visited) (ox, oy) ->
                    if ductSystem.[ox, oy] = '#' || Set.contains (ox, oy) visited then
                        acc, visited
                    else
                        (ox, oy)::acc, Set.add (ox, oy) visited))
        if List.contains goal newEdges then steps + 1
        else
            searcher newEdges newVisited (steps + 1)
    searcher [start] Set.empty 0

let rec possibles acc rem =
    [
        if Set.count rem = 0 then
            yield acc
        for next in Set.toList rem do
            yield! possibles (next::acc) (Set.remove next rem)
    ]
        
let mutable dists = Map.empty

let length path = 
    ((0, List.head path), List.tail path)
    ||> List.fold (fun (acc, last) next ->
        if Map.containsKey (last, next) dists then
            acc + dists.[last, next], next
        elif Map.containsKey (next, last) dists then
            acc + dists.[next, last], next
        else
            let dist = bfs last next
            dists <- Map.add (last, next) dist dists
            acc + dist, next)
    |> fst

let part1 () =
    let allPaths = possibles [start] (Set.ofList numbers)
    allPaths |> List.map length |> List.min

let part2 () =
    let allPaths = (possibles [start] (Set.ofList numbers)) |> List.map (fun p -> start::p)
    allPaths |> List.map length |> List.min
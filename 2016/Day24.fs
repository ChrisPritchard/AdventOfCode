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

module Day24

//let input = System.IO.File.ReadAllLines "Day24-input.txt"
let input = 
    [|
        "###########"
        "#0.1.....2#"
        "#.#######.#"
        "#4.......3#"
        "###########"
    |]

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

let part1 () =
    
    let rec possibles acc rem =
        [
            if Set.count rem = 0 then
                yield acc
            for next in Set.toList rem do
                yield! possibles (next::acc) (Set.remove next rem)
        ]            
    
    let allPaths = possibles [start] (Set.ofList numbers)
    0
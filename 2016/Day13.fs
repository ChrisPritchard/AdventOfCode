(*
--- Day 13: A Maze of Twisty Little Cubicles ---

You arrive at the first floor of this new building to discover a much less welcoming environment than the shiny atrium of the last one. Instead, you are in a maze of twisty little cubicles, all alike.

Every location in this area is addressed by a pair of non-negative integers (x,y). Each such coordinate is either a wall or an open space. You can't move diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward positive x and y; negative values are invalid, as they represent a location outside the building. You are in a small waiting area at 1,1.

While it seems chaotic, a nearby morale-boosting poster explains, the layout is actually quite logical. You can determine whether a given x,y coordinate will be a wall or an open space using a simple system:

    Find x*x + 3*x + 2*x*y + y + y*y.
    Add the office designer's favorite number (your puzzle input).
    Find the binary representation of that sum; count the number of bits that are 1.
        If the number of bits that are 1 is even, it's an open space.
        If the number of bits that are 1 is odd, it's a wall.

For example, if the office designer's favorite number were 10, drawing walls as # and open spaces as ., the corner of the building containing 0,0 would look like this:

  0123456789
0 .#.####.##
1 ..#..#...#
2 #....##...
3 ###.#.###.
4 .##..#..#.
5 ..##....#.
6 #...##.###

Now, suppose you wanted to reach 7,4. The shortest route you could take is marked as O:

  0123456789
0 .#.####.##
1 .O#..#...#
2 #OOO.##...
3 ###O#.###.
4 .##OO#OO#.
5 ..##OOO.#.
6 #...##.###

Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current location, 1,1).

What is the fewest number of steps required for you to reach 31,39?
*)

(*
--- Part Two ---

How many locations (distinct x,y coordinates, including your starting location) can you reach in at most 50 steps?
*)

module Day13

let input = 1350

let isOpen (x, y) = 
    if x < 0 || y < 0 then false 
    else
        let number = (x*x) + (3*x) + (2*x*y) + y + (y*y) + input
        let rec countOnes n c = 
            if n <= 0 then c % 2 = 0
            else
                let c = if n % 2 = 1 then c + 1 else c
                countOnes (n >>> 1) c
        countOnes number 0 

let adjacent x y = 
    [x-1,y;x+1,y;x,y-1;x,y+1]

let part1 () =
    let rec bfs positions visited (tx, ty) s =
        if List.contains (tx, ty) positions then s
        else
            let (nextVisited, nextPositions) =
                ((visited, []), positions) 
                ||> List.fold (fun (visited, acc) (x, y) ->
                    ((visited, acc), adjacent x y |> List.filter isOpen) 
                    ||> List.fold (fun (visited, acc) p -> 
                        if Set.contains p visited then
                            visited, acc
                        else
                            Set.add p visited, p::acc))
            bfs nextPositions nextVisited (tx, ty) (s + 1)

    bfs [1,1] (Set.empty.Add (1,1)) (31,39) 0

let part2 () =
    let rec bfs positions visited s smax =
        if s = smax then Set.count visited
        else
            let (nextVisited, nextPositions) =
                ((visited, []), positions) 
                ||> List.fold (fun (visited, acc) (x, y) ->
                    ((visited, acc), adjacent x y |> List.filter isOpen) 
                    ||> List.fold (fun (visited, acc) p -> 
                        if Set.contains p visited then
                            visited, acc
                        else
                            Set.add p visited, p::acc))
            bfs nextPositions nextVisited (s + 1) smax

    bfs [1,1] (Set.empty.Add (1,1)) 0 50
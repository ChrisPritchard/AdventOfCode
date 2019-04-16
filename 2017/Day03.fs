﻿(*
--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
*)

module Day03

let input = 312051

type Dir = R | U | L | D

let part1 () =
 //(* 65	64	63	62	61	60	59	58	57				32
 //   66	37	36	35	34	33	32	31	56				24
 //   67	38	17	16	15	14	13	30	55				16
 //   68	39	18	5	4	3	12	29	54				8
 //   69	40	19	6	1	2	11	28	53				1
 //   70	41	20	7	8	9	10	27	52				
 //   71	42	21	22	23	24	25	26	51				
 //   72	43	44	45	46	47	48	49	50				
 //   73	74	75	76	77	78	79	80	81 *)
 //   let ring = (input - 1) / 8
 //   let ringSize = ring * 8
 //   let ringStart = (ring - 1) * 8
 //   let sides = (ringSize - 4) / 4
 //   let topBottom = sides + 2
    let rec mapper (x, y) dir map n =
        let nextMap = Map.add (x, y) n map
        if n = input then x, y
        else
            let nextPos, nextDir = 
                match dir with
                | R when not (Map.containsKey (x, y - 1) map) -> (x, y - 1), U
                | U when not (Map.containsKey (x - 1, y) map) -> (x - 1, y), L
                | L when not (Map.containsKey (x, y + 1) map) -> (x, y + 1), D
                | D when not (Map.containsKey (x + 1, y) map) -> (x + 1, y), R
                | R -> (x + 1, y), R
                | U -> (x, y - 1), U
                | L -> (x - 1, y), L
                | D -> (x, y + 1), D
            mapper nextPos nextDir nextMap (n + 1)

    let (x, y) = mapper (0, 0) D Map.empty 1

    abs x + abs y

let part2 () = 0
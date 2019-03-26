(*
--- Day 19: An Elephant Named Joseph ---

The Elves contact you over a highly secure emergency channel. Back at the North Pole, the Elves are busy misunderstanding White Elephant parties.

Each Elf brings a present. They all sit in a circle, numbered starting with position 1. Then, starting with the first Elf, they take turns stealing all the presents from the Elf to their left. An Elf with no presents is removed from the circle and does not take turns.

For example, with five Elves (numbered 1 to 5):

  1
5   2
 4 3

    Elf 1 takes Elf 2's present.
    Elf 2 has no presents and is skipped.
    Elf 3 takes Elf 4's present.
    Elf 4 has no presents and is also skipped.
    Elf 5 takes Elf 1's two presents.
    Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
    Elf 3 takes Elf 5's three presents.

So, with five Elves, the Elf that sits starting in position 3 gets all the presents.

With the number of Elves given in your puzzle input, which Elf gets all the presents?
*)

(*
--- Part Two ---

Realizing the folly of their present-exchange rules, the Elves agree to instead steal presents from the Elf directly across the circle. If two Elves are across the circle, the one on the left (from the perspective of the stealer) is stolen from. The other rules remain unchanged: Elves with no presents are removed from the circle entirely, and the other elves move in slightly to keep the circle evenly spaced.

For example, with five Elves (again numbered 1 to 5):

    The Elves sit in a circle; Elf 1 goes first:

      1
    5   2
     4 3

    Elves 3 and 4 are across the circle; Elf 3's present is stolen, being the one to the left. Elf 3 leaves the circle, and the rest of the Elves move in:

      1           1
    5   2  -->  5   2
     4 -          4

    Elf 2 steals from the Elf directly across the circle, Elf 5:

      1         1 
    -   2  -->     2
      4         4 

    Next is Elf 4 who, choosing between Elves 1 and 2, steals from Elf 1:

     -          2  
        2  -->
     4          4

    Finally, Elf 2 steals from Elf 4:

     2
        -->  2  
     -

So, with five Elves, the Elf that sits starting in position 2 gets all the presents.

With the number of Elves given in your puzzle input, which Elf now gets all the presents?
*)

module Day19

let input = 5//3004953

let part1 () =
    
    let start = [|1..input|]
    
    let rec whiteElephants ring = 
        let next = 
            ring 
            |> Seq.indexed 
            |> Seq.choose (fun (i, v) -> 
                if i % 2 = 0 then Some v else None)
            |> Seq.toArray
        if next.Length = 1 then next.[0]
        else
            let next = 
                if Seq.length ring % 2 = 1 
                then next.[1..] else next
            whiteElephants next
    
    whiteElephants start

type Elf = {
    index: int
    mutable left: Elf
    mutable right: Elf
}

let part2 () =
    
    let counter total =
        let mutable length = total
        let mutable current = { index = 1; left = Unchecked.defaultof<Elf>; right = Unchecked.defaultof<Elf> }
        let start = current

        for i = 2 to length do
            let newElf = { index = i; left = current; right = Unchecked.defaultof<Elf> }
            current.right <- newElf
            current <- newElf

        current.right <- start
        start.left <- current
        current <- start

        while length > 1 do
            let across = length / 2
            let mutable target = current
            for n = 1 to across do 
                target <- target.right
            target.left.right <- target.right
            target.right.left <- target.left
            current <- current.right
            length <- length - 1
    
        current.index

    let test = [1..100] |> List.map counter
    let ones = test |> List.indexed |> List.filter (snd >> (=) 1)

    let heuristic total =
        let closest3 = 
            Seq.initInfinite (fun i -> i, pown 3 i) 
            |> Seq.find (fun (_, v) -> v >= total) 
            |> fun (i, _) -> 
                pown 3 (i - 1)
        total - closest3

    let test2 = [1..100] |> List.map heuristic
    
    0
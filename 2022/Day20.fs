module Day20

open Common

// a single array with iterative swapping seems best

let part1 () =
    let ring = 
        readEmbedded "day20"
        |> Array.map int
    let positions = [|0..ring.Length - 1|]

    let insert value index =
        let rec flip prev next =
            let next = if next = ring.Length then 0 else next
            let current = positions[next]
            positions[next] <- prev
            let next = next + 1
            if next = index then ()
            else
                flip current next
        flip positions[index] (index + 1)
        positions[index] <- value

    for i in [0..ring.Length-1] do
        printfn "%A" positions
        let pos = Array.findIndex ((=) i) positions
        let adjust = if ring[i] >= 0 then ring[i] + 1 else ring[i] - 1
        let target = (pos + adjust) % ring.Length
        insert i target
        
    positions |> Array.map (fun i -> ring[i])

let part2 () =
    0
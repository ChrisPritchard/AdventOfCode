module Day20

open Common

// a single array with iterative swapping seems best
// 0 1 2 3 4 5 6 7
// 1 2 0 3 4 5 6 7 <- moving 0 to position 2
// the shift is to back slide all values between the two indices: from start index + 1 to end, move current to prior
// it would be forward sliding if moving back:
// 0 1 2 3 4 5 6 7
// 0 1 2 7 3 4 5 6 <- moving 7 to position 3

let part1 () =
    let ring = 
        readEmbedded "day20"
        |> Array.map int
    let positions = [|0..ring.Length - 1|]

    let index i = if i = -1 then positions.Length - 1 else if i = positions.Length then 0 else i

    let rec slide dir current prev last =
        match prev with
        | None -> slide dir (index (current + dir)) (Some positions[current]) last
        | Some prevValue ->
            printfn "setting pos %d to %d" current prevValue
            let newPrev = Some positions[current]
            positions[current] <- prevValue
            if current = last then ()
            else
                slide dir (index (current + dir)) newPrev last

    for i in [0..ring.Length-1] do
        printfn "before %d moving %d: %A" i ring[i] positions
        let pos = Array.findIndex ((=) i) positions
        let target = (pos + ring[i]) % ring.Length
        let target = if target < 0 then ring.Length + target else target
        printfn "span is %d - %d" pos target
        if target >= pos then slide -1 target None pos else slide 1 pos None target
        printfn "finally setting pos %d to %d" target i
        positions[target] <- i
        printfn "final state: %A" positions
        printfn "real state:  %A" (positions |> Array.map (fun i -> ring[i]))
        
    positions |> Array.map (fun i -> ring[i])

let part2 () =
    0
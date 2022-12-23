module Day20

open Common

let part1 () =
    let ring = 
        readEmbedded "day20"
        |> Array.map int

    let normalise (left, right) =
        let l, r = left % ring.Length, right % ring.Length
        (if l < 0 then ring.Length + l else l), (if r < 0 then ring.Length + r else r)

    let indexToNeighbours = 
        [|0..ring.Length - 1|] 
        |> Array.map (fun i -> i, normalise (i-1, i+1))
        |> dict
    let neighboursToIndex = indexToNeighbours |> Seq.map (fun kv -> kv.Value, kv.Key) |> dict

    let rec findTarget (left, right) amount =
        if amount = 0 then
            (left, right)
        else if amount < 0 then 
            findTarget indexToNeighbours[left] (amount + 1)
        else
            findTarget indexToNeighbours[right] (amount - 1)

    for i in [0..ring.Length - 1] do
        let amount = ring[i]
        if amount <> 0 then
            let (left, right) = indexToNeighbours[i]
            let (tleft, tright) = findTarget (left, right) amount

            // removing from current left
            let (leftleft, _) = indexToNeighbours[left]
            let newLeft = (leftleft, right)
            indexToNeighbours[left] <- newLeft
            neighboursToIndex[newLeft] <- left

            // removing from current right
            let (_, rightright) = indexToNeighbours[right]
            let newright = (left, rightright)
            indexToNeighbours[right] <- newright
            neighboursToIndex[newright] <- right

            // adding to target left
            let (leftleft, _) = indexToNeighbours[tleft]
            let newLeft = (leftleft, i)
            indexToNeighbours[tleft] <- newLeft
            neighboursToIndex[newLeft] <- tleft

            // adding to target right
            let (_, rightright) = indexToNeighbours[tright]
            let newright = (i, rightright)
            indexToNeighbours[tright] <- newright
            neighboursToIndex[newright] <- tright

    let index0 = Array.findIndex ((=) 0) ring
    (indexToNeighbours[index0], [|0..ring.Length - 1|])
    ||> Array.mapFold (fun (_, right) _ -> right, indexToNeighbours[right])

let part2 () =
    0
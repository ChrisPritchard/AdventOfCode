module Day20

open Common
open System.Collections.Generic

let part1 () =
    let ring = 
        readEmbedded "day20"
        |> Array.map int64

    let normalise (left, right) =
        let l, r = left % ring.Length, right % ring.Length
        (if l < 0 then ring.Length + l else l), (if r < 0 then ring.Length + r else r)

    let indexToNeighbours = 
        [|0..ring.Length - 1|] 
        |> Array.map (fun i -> i, normalise (i-1, i+1))
        |> dict |> Dictionary<int, int * int>
    let neighboursToIndex = indexToNeighbours |> Seq.map (fun kv -> kv.Value, kv.Key) |> dict |> Dictionary<int * int, int>

    let asArray startIndex mapToRing =
        (indexToNeighbours[startIndex], [|0..ring.Length - 1|])
        ||> Array.mapFold (fun neighbours _ -> 
            (if mapToRing then ring[neighboursToIndex[neighbours]] else neighboursToIndex[neighbours]), indexToNeighbours[snd neighbours])
        |> fst

    let rec findTarget (left, right) amount =
        if amount = 0 then
            neighboursToIndex[left, right]
        else if amount < 0 then 
            findTarget indexToNeighbours[left] (amount + 1)
        else
            findTarget indexToNeighbours[right] (amount - 1)

    let update index neighbours = 
        indexToNeighbours[index] <- neighbours
        neighboursToIndex[neighbours] <- index

    let removeElement (elementLeftNeighbour, elementRightNeighbour) =
        let (leftNeighbourLeft, _) = indexToNeighbours[elementLeftNeighbour]
        let newLeft = (leftNeighbourLeft, elementRightNeighbour)
        update elementLeftNeighbour newLeft

        let (_, rightNeighbourRight) = indexToNeighbours[elementRightNeighbour]
        let newright = (elementLeftNeighbour, rightNeighbourRight)
        update elementRightNeighbour newright

    for i in [0..ring.Length-1] do
        let amount = ring[i]
        if amount <> 0L then
            let left, right = indexToNeighbours[i] // current position - by binding the neighbours together we 'remove' the value we are moving
            removeElement (left, right)   

            let target = findTarget (left, right) (if amount < 0 then amount - 1 else amount + 1)            
            let targetLeft, targetRight = indexToNeighbours[target]      

            if amount > 0 then 
                let targetLeftLeft, _ = indexToNeighbours[targetLeft]
                let newLeftNeighbours = targetLeftLeft, i
                update targetLeft newLeftNeighbours
                let indexNeighbours = targetLeft, target
                update i indexNeighbours
                let newTargetNeighbours = i, targetRight
                update target newTargetNeighbours        
            else
                let newTargetNeighbours = targetLeft, i
                update target newTargetNeighbours
                let indexNeighbours = target, targetRight
                update i indexNeighbours
                let _, targetRightRight = indexToNeighbours[targetRight]
                let newRightNeighbours = i, targetRightRight
                update targetRight newRightNeighbours 

    let final = asArray (Array.findIndex ((=) 0) ring) true
    [1000;2000;3000] |> List.sumBy (fun n -> final[n % final.Length])

let part2 () =
    0
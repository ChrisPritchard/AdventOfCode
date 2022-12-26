module Day20

open Common
open System.Collections.Generic

let mix count (ring: int64[]) =
    let length = ring.Length

    let normalise (left, right) =
        let l, r = left % length, right % length
        (if l < 0 then length + l else l), (if r < 0 then length + r else r)

    let indexToNeighbours = 
        [|0..length - 1|] 
        |> Array.map (fun i -> i, normalise (i-1, i+1))
        |> dict |> Dictionary<int, int * int>
    let neighboursToIndex = indexToNeighbours |> Seq.map (fun kv -> kv.Value, kv.Key) |> dict |> Dictionary<int * int, int>

    let asArray startIndex mapToRing =
        (indexToNeighbours[startIndex], [|0..length - 1|])
        ||> Array.mapFold (fun neighbours _ -> 
            (if mapToRing then ring[neighboursToIndex[neighbours]] else neighboursToIndex[neighbours]), indexToNeighbours[snd neighbours])
        |> fst

    let rec findTarget (left, right) nextFunc amount =
        if amount = 0L then
            neighboursToIndex[left, right]
        else 
            findTarget (indexToNeighbours[nextFunc (left, right)]) nextFunc (amount - 1L)

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

    for _ in [0..count-1] do
        for i in [0..length-1] do
            let amount = ring[i]
            if amount <> 0 then
                let left, right = indexToNeighbours[i] // current position - by binding the neighbours together we 'remove' the value we are moving
                removeElement (left, right)   

                // move one more in the target direction. but first, if the number is greater than the length of the array 
                // get the final distance by modding out the array length (-1 as we've removed an element)
                let toTravel = ((abs amount) % int64 (length - 1)) + 1L
                let nextFunc = if amount < 0 then fst else snd
                let target = findTarget (left, right) nextFunc toTravel
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

    asArray (Array.findIndex ((=) 0L) ring) true

let part1 () =
    let ring = 
        readEmbedded "day20"
        |> Array.map int64

    let final = mix 1 ring
    [1000;2000;3000] |> List.sumBy (fun n -> final[n % final.Length])

let part2 () =
    let ring = 
        readEmbedded "day20"
        |> Array.map (int64 >> (*) 811589153L)

    let final = mix 10 ring
    [1000;2000;3000] |> List.sumBy (fun n -> final[n % final.Length])
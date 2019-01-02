module Compiled

// let mutable private reg = [|1;0;0;0;0;0|]

// let addTo0 added =
//     reg.[0] <- reg.[0] + added

// let private add1to2 () =
//     reg.[2] <- reg.[2] + 1

// let private add1to1 () =
//     reg.[1] <- reg.[1] + 1

// let part1Start = [|0;1;1;1025;0|]
// let part2Start = [|1;1;1;10551425;0|]

// let rec private proc3 () = 
//     if reg.[1] * reg.[2] = reg.[3] then 
//         addTo0 reg.[1]
//     add1to2 ()
//     if reg.[2] > reg.[3] then
//         add1to1 ()
//         if reg.[1] > reg.[3] then
//             reg.[0]
//         else
//             reg.[2] <- 1
//             proc3 ()
//     else
//         proc3 ()

// the specified program ultimately sets reg3 based on reg0 being 0 or 1
// then runs the proc3 loop above, which sums all factors up to reg 3 

let part1Start = 1025
let part2Start = 10551425

let runCompiled reg3 =
    [1..reg3] 
    |> List.filter (fun n -> reg3 % n = 0) 
    |> List.sum
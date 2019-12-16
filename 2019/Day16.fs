module Day16

open Common
open System.IO

let input = File.ReadAllText "./inputs/day16.txt"

let part1 () =

    let run (input: int[]) =
        [|0..input.Length - 1|] 
        |> Array.map (fun i ->
            let total = 
                Array.skip i input 
                |> Array.chunkBySize (i + 1) 
                |> Array.mapi (fun i a -> 
                    match i % 4 with
                    | 0 -> Array.sum a
                    | 2 -> -1 * Array.sum a
                    | _ -> 0)
                |> Array.sum
            abs (total % 10))

    let rec runner current count =
        if count = 0 then
            current |> Array.truncate 8 |> Array.map (string >> char) |> asString
        else
            runner (run current) (count - 1)
    
    let ti = input |> Seq.map (string >> int) |> Seq.toArray
    runner ti 100

(*
For part 2 below I had to go to reddit again to get the crucial insight, which is obvious really. 
I'll explain it here how I thought through it, which resulted in the code below:

- Any given digit is only calculated based on the numbers after it (except for the first digit);
    this is because 0 is repeated by the digit index, plus 1, which skips all digits up to and including the target.
- The 'offset' for part 2 is over the half way mark. This means that after the first zero, the next character in the base pattern '1'
    will be repeated for the rest of the number. The offset is large enough that the third base pattern '0' is never reached.

Based on these two insights, and given we only need to consider the number at the offset for our result, we can simplify the run:

- Only consider the digits from the offset onwards: the entire number up to this point can be ignored as it will have no affect on the offset digits.
    This changes a 6.5 million digit number into a ~500k number.
- Each digit in the reduced number is simply the sum of all numbers after it, mod 10.

The solution therefore, presented below, is to:

- Create the ~500k number (which is just the input, reversed, repeated 800 odd times, with a final append of the remainder). This is the new input.
- Create a runner that computes the partial sums in reverse. scanBack in F# does this nicely.
- Map the result mod 10.

After a 100 runs, since we have already snipped off the irrelevant initial six million odd digits, the result is just like part 1: first 8 characters of the final number.
*)

let part2 () =
    
    let ti = input |> Seq.rev |> Seq.map (string >> int) |> Seq.toArray
    
    let offset = int64 input.[0..6]
    let totalLen = int64 ti.Length * 10000L
    let toConsider = totalLen - offset

    let replications = float toConsider / float ti.Length |> floor |> int
    let replicated = Array.init replications (fun _ -> Array.copy ti) |> Array.collect id
    let remainder = toConsider - int64 replicated.Length |> int
    let final = Array.append replicated ti.[0..remainder-1] |> Array.rev

    let run (input: int[]) =
        Array.scanBack (+) input 0 |> Array.map (fun n -> n % 10)

    let rec runner current count =
        if count = 0 then
            current |> Array.truncate 8 |> Array.map (string >> char) |> asString
        else
            runner (run current) (count - 1)

    runner final 100
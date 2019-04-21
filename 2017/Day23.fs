(*
--- Day 23: Coprocessor Conflagration ---

You decide to head directly to the CPU and fix the printer from there. As you get close, you find an experimental coprocessor doing so much work that the local programs are afraid it will halt and catch fire. This would cause serious issues for the rest of the computer, so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on that tablet. The general functionality seems very similar, but some of the instructions are different:

    set X Y sets register X to the value of Y.
    sub X Y decreases register X by the value of Y.
    mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
    jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

    Only the instructions listed above are used. The eight registers here, named a through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows for testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul instruction invoked?

--- Part Two ---

Now, it's time to fix the problem.

The debug mode switch is wired directly to register a. You flip the switch, which makes register a now start at 1 when the program is executed.

Immediately, the coprocessor begins to overheat. Whoever wrote this program obviously didn't choose a very efficient implementation. You'll need to optimize the program if it has any hope of completing before Santa needs that printer working.

The coprocessor's ultimate goal is to determine the final value left in register h once the program completes. Technically, if it had that... it wouldn't even need to run the program.

After setting register a to 1, if the program were to run to completion, what value would be left in register h?

Although it hasn't changed, you can still get your puzzle in*)

module Day23

open Common

let input = System.IO.File.ReadAllLines "./inputs/day23.txt"

let inline regVal reg registers =
    Map.tryFind reg registers |> Option.defaultValue 0L

type Inst = 
    | Set of reg: char * value: Source
    | Sub of reg: char * value: Source
    | Mul of reg: char * value: Source
    | Jnz of reg: char * value: Source
    | Jump of value: Source
and Source = Reg of char | Value of int64

let getSource (text: string) =
    if System.Char.IsLetter text.[0] then Reg (text.[0]) else Value (int64 text)

let getSourceValue registers =
    function
    | Value n -> n
    | Reg c -> regVal c registers

let instructions =
    Array.map (fun line ->
        match split " " line with
        | [|"set";reg;regOrVal|] -> Set (reg.[0], getSource regOrVal)
        | [|"sub";reg;regOrVal|] -> Sub (reg.[0], getSource regOrVal)
        | [|"mul";reg;regOrVal|] -> Mul (reg.[0], getSource regOrVal)
        | [|"jnz";test;regOrVal|] -> 
            match getSource test with
            | Reg r -> Jnz (r, getSource regOrVal)
            | Value v -> if v <> 0L then Jump (getSource regOrVal) else Jump (Value 1L)
        | _ -> failwith "unrecognised instruction") input

let part1 () =

    let rec processor index registers mulCount =
        if index < 0 || index >= instructions.Length then
            mulCount
        else
            match instructions.[index] with
            | Jump amount ->
                processor (index + int (getSourceValue registers amount)) registers mulCount
            | Jnz (register, amount) ->
                if regVal register registers = 0L then
                    processor (index + 1) registers mulCount
                else
                    processor (index + int (getSourceValue registers amount)) registers mulCount
            | Set (target, source) ->
                let registers = Map.add target (getSourceValue registers source) registers
                processor (index + 1) registers mulCount
            | Sub (target, source) ->
                let newVal = regVal target registers - getSourceValue registers source
                let registers = Map.add target newVal registers
                processor (index + 1) registers mulCount
            | Mul (target, source) ->
                let newVal = regVal target registers * getSourceValue registers source
                let registers = Map.add target newVal registers
                processor (index + 1) registers (mulCount + 1)

    processor 0 Map.empty 0

let part2 () =
    
    // based on analysis, count of non-primes between 106500 and 123500

    // prime tester from here: http://www.fssnip.net/7E/title/Prime-testing
    let isPrime n =
        match n with
        | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
        | _ ->
            let maxDiv = int(System.Math.Sqrt(float n)) + 1
            let rec f d i = 
                if d > maxDiv then 
                    true
                else
                    if n % d = 0 then 
                        false
                    else
                        f (d + i) (6 - i)     
            f 5 2
    
    [106500..17..123500] |> List.filter (isPrime >> not) |> List.length
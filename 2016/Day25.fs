(*
--- Day 25: Clock Signal ---

You open the door and find yourself on the roof. The city sprawls away from you for miles and miles.

There's not much time now - it's already Christmas, but you're nowhere near the North Pole, much too far to deliver these stars to the sleigh in time.

However, maybe the huge antenna up here can offer a solution. After all, the sleigh doesn't need the stars, exactly; it needs the timing data they provide, and you happen to have a massive signal generator right here.

You connect the stars you have to your prototype computer, connect that to the antenna, and begin the transmission.

Nothing happens.

You call the service number printed on the side of the antenna and quickly explain the situation. "I'm not sure what kind of equipment you have connected over there," he says, "but you need a clock signal." You try to explain that this is a signal for a clock.

"No, no, a clock signal - timing information so the antenna computer knows how to read the data you're sending it. An endless, alternating pattern of 0, 1, 0, 1, 0, 1, 0, 1, 0, 1...." He trails off.

You ask if the antenna can handle a clock signal at the frequency you would need to use for the data from the stars. "There's no way it can! The only antenna we've installed capable of that is on top of a top-secret Easter Bunny installation, and you're definitely not-" You hang up the phone.

You've extracted the antenna's clock signal generation assembunny code (your puzzle input); it looks mostly compatible with code you worked on just recently.

This antenna code, being a signal generator, uses one extra instruction:

    out x transmits x (either an integer or the value of a register) as the next value for the clock signal.

The code takes a value (via register a) that describes the signal to generate, but you're not sure how it's used. You'll have to find the input to produce the right signal through experimentation.

What is the lowest positive integer that can be used to initialize register a and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?
*)

module Day25

open Common

let input = System.IO.File.ReadAllLines "Day25-input.txt"

type Instruction =
    | CopyValue of int * char
    | CopyRegister of char * char
    | Increment of char
    | Decrement of char
    | Jump of int
    | JumpNotZero of char * int
    | Out of char

let parseInstruction text = 
    let segments = split " " text
    match segments.[0] with
    | "out" -> Out segments.[1].[0]
    | "inc" -> Increment segments.[1].[0]
    | "dec" -> Decrement segments.[1].[0]
    | "jnz" -> 
        if System.Char.IsLetter segments.[1].[0] then
            JumpNotZero (segments.[1].[0], int segments.[2])
        else
            Jump (if int segments.[1] = 0 then 1 else int segments.[2])
    | "cpy" | _ -> 
        if System.Char.IsLetter segments.[1].[0] then
            CopyRegister (segments.[1].[0], segments.[2].[0])
        else
            CopyValue (int segments.[1], segments.[2].[0])

let getRegister r registers = 
    Map.tryFind r registers |> Option.defaultValue 0

let instructions = input |> Array.map parseInstruction

let rec runInstruction registers res i n =
    if i >= instructions.Length || i < 0 || n = 0 then
        res
    else
        match instructions.[i] with
        | Out r ->
            runInstruction registers ((getRegister r registers)::res) (i + 1) (n - 1)
        | Increment r -> 
            let next = registers |> getRegister r |> fun e -> Map.add r (e + 1) registers
            runInstruction next res (i + 1) (n - 1)
        | Decrement r ->
            let next = registers |> getRegister r |> fun e -> Map.add r (e - 1) registers
            runInstruction next res (i + 1) (n - 1)
        | CopyValue (v, r) ->
            let next = Map.add r v registers
            runInstruction next res (i + 1) (n - 1)
        | CopyRegister (ra, r) ->
            let next = Map.add r (getRegister ra registers) registers
            runInstruction next res (i + 1) (n - 1)
        | Jump v ->
            runInstruction registers res (i + v) (n - 1)
        | JumpNotZero (r, v) ->
            if  getRegister r registers = 0 then
                runInstruction registers res (i + 1) (n - 1)
            else
                runInstruction registers res (i + v) (n - 1)

let part1 () =

    let target = [1..100] |> List.map (fun i -> i % 2)
    let res = 
        [1..100000] 
        |> List.map (fun a -> a, runInstruction (Map.empty.Add ('a', a)) [] 0 100000)
        |> List.filter (fun (_, l) -> List.length l > 0 && List.truncate 100 l = target)

    //cpy a d           d is a
    //cpy 15 c          c 15
    //cpy 170 b         b 170
        //inc d             
        //dec b          
        //jnz b -2
        //dec c
        //jnz c -5      d = a + (15 * 170) // 2550
    //cpy d a           a = a + 2550
    //jnz 0 0           nothing
    //cpy a b           b is a + 2550        
    //cpy 0 a           a is 0
    //cpy 2 c           c is 2
        //jnz b 2             
        //jnz 1 6
        //dec b             
        //dec c
        //jnz c -4      b = b - 2
            //inc a         
            //jnz 1 -7  a = a + (b / 2) = a + (a + 2548 / 2)
    //cpy 2 b // b = 2
        //jnz c 2 // c = 0?
        //jnz 1 4
        //dec b     
        //dec c
        //jnz 1 -4
    //jnz 0 0
    //27: out b
    //jnz a -19
    //jnz 1 -21

    0
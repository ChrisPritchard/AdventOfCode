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

(*
--- Part Two ---

The antenna is ready. Now, all you need is the fifty stars required to generate the signal for the sleigh, but you don't have enough.

You look toward the sky in desperation... suddenly noticing that a lone star has been installed at the top of the antenna! Only 49 more to go.

You have enough stars to TRANSMIT THE SYGNAL!
.*)

module Day25

let part1 () =

    let runProgram x =
        seq {
            let mutable a, b, c, d = x, 0, 0, 0
            d <- a + 2550
            //0:  cpy a d
            //1:  cpy 15 c
            //2:  cpy 170 b
            //3:  inc d
            //4:  dec b
            //5:  jnz b -2
            //6:  dec c
            //7:  jnz c -5
            while true do
                a <- d //8:  cpy d a
                while a > 0 do    //9:  jnz 0 0
                    b <- a//10: cpy a b
                    a <- 0//11: cpy 0 a
                    c <- 2//12: cpy 2 c
                    
                    
                    while b > 0 do//13: jnz b 2
                    //14: jnz 1 6
                        b <- b - 1 //15: dec b
                        c <- c - 1 //16: dec c
                        if c = 0 then //17: jnz c -4
                            a <- a + 1//18: inc a
                            c <- 2//19: jnz 1 -7 
                    
                    b <- 2 //20: cpy 2 b
                    while c > 0 do//21: jnz c 2 //22: jnz 1 4 //25: jnz 1 -4
                        b <- b - 1 //23: dec b
                        c <- c - 1 //24: dec c
                    //26: jnz 0 0
                    yield b //27: out b
                    //28: jnz a -19
                //29: jnz 1 -21
        }

    Seq.initInfinite (fun i -> 
        (i + 1), Seq.take 10 (runProgram (i + 1)) |> Seq.map string |> String.concat "")
    |> Seq.find (fun (_, s) -> s = "0101010101")
    |> fst
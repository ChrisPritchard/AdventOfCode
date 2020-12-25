module Day25

open System.IO

let input = File.ReadAllLines "./inputs/day25.txt"

let christmasDay () =
    let a, b = uint64 input.[0], uint64 input.[1]
    
    let transform n sn =
        let step1 = n * sn
        step1 % 20201227UL
    
    let rec loopSize n c sn target =
        if n = target then c
        else loopSize (transform n sn) (c+1) sn target
    
    let loops = loopSize 1UL 0 7UL a

    let rec transformByLoop n sn c =
        if c = 0 then n
        else transformByLoop (transform n sn) sn (c-1)

    transformByLoop 1UL b loops
    
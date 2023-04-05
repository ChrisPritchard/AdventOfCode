module Day25

open Common

let part1 () =
    
    let input = readEmbedded "day25"

    let mutable sum = 0L
    for line in input do 
        let line = Seq.rev line |> Seq.indexed
        for (index, char) in line do
            let value = match char with | '=' -> -2L | '-' -> -1L | '0' -> 0L | '1' -> 1L | '2' | _ -> 2L
            let value = value * (pown 5L index)
            sum <- sum + value

    let rec snafu acc dec = 
        if dec = 0L then acc
        else
            let rem, dec = dec % 5L, dec / 5L
            let s = 
                match rem with
                | 4L -> "-"
                | 3L -> "="
                | 2L -> "2"
                | 1L -> "1"
                | 0L | _ -> "0"
            let dec = if rem = 4L || rem = 3L then dec + 1L else dec
            snafu (s + acc) dec
    
    snafu "" sum

module Day13

open System.IO

let input = (File.ReadAllText ("./inputs/day13.txt")).Split ','

let part1 () =    
    
    let mem = Intcode.memFrom input    
    let io = Intcode.IO.create ()
    Intcode.run 0L 0L mem io |> ignore

    Seq.chunkBySize 3 io.output
    |> Seq.map Seq.toArray
    |> Seq.filter (fun a -> a.[2] = 2L)
    |> Seq.length

let part2 () =

    let mem = Intcode.memFrom input    
    let io = Intcode.IO.create ()
    mem.[0L] <- 2L
    
    let rec looper ip rb mem =
        let state, ip, rb, mem = Intcode.run ip rb mem io
        
        let blocks, score =
            Seq.chunkBySize 3 io.output
            |> Seq.map Seq.toArray
            |> Seq.fold (fun (blocks, score) a -> 
                if a.[0] = -1L && a.[1] = 0L then blocks, int a.[2]
                else if a.[2] = 2L then blocks + 1, score
                else blocks, score) (0, 0)

        if blocks = 0 then score
        else if state = Intcode.Halted then 
            io.clear ()
            looper 0L 0L mem
        else
            io.write 0L
            looper ip rb mem
            
    looper 0L 0L mem
    
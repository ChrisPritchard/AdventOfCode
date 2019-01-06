
[<EntryPoint>]
let main _ =

    let row, column = 2978, 3083

    let rec findCode row column = 
        let rec looper r c prev =
            let next =
                if r = 1 && c = 1 then 20151125L
                else
                    (prev * 252533L) % 33554393L
            if r = row && c = column then next
            else
                let nr, nc = if r = 1 then c + 1, 1 else r - 1, c + 1
                looper nr nc next
        looper 1 1 0L

    printfn "part 1: %i" <| findCode row column

    0

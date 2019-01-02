
[<EntryPoint>]
let main _ =
    let input = 793061
    let sequence = string input |> Seq.map (fun c -> int c - int '0') |> Seq.toArray

    let mutable length = 2
    let max = 50000000
    let array = Array.zeroCreate max
    array.[0] <- 3
    array.[1] <- 7
    let mutable i1, i2 = 0, 1
    while length < max do
        let num1 = array.[i1]
        let num2 = array.[i2]
        let sum = num1 + num2
        if sum < 10 then
            array.[length] <- sum
            length <- length + 1
        else
            array.[length] <- sum / 10
            array.[length+1] <- sum % 10
            length <- length + 2
        i1 <- (i1 + 1 + num1) % length
        i2 <- (i2 + 1 + num2) % length

    printfn "part 1: %s" <| (array.[input..input+9] |> Array.map string |> String.concat "")
    printfn "part 2: %i" <| (array |> Array.windowed (sequence.Length) |> Array.findIndex ((=) sequence))

    0

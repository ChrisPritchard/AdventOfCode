
[<EntryPoint>]
let main _ =
    let input = "ckczppom"

    use md5 = System.Security.Cryptography.MD5.Create()
    let gethash i = 
        let text = input + string i
        let bytes = System.Text.Encoding.UTF8.GetBytes text
        let cipher = md5.ComputeHash bytes
        cipher |> Seq.map (sprintf "%02X") |> String.concat ""

    let part1 = 
        Seq.initInfinite id |> Seq.find (fun i -> 
        let hash = gethash i
        hash.[0..4] = "00000")
    printfn "part 1: %i" part1

    let part2 = 
        Seq.initInfinite id |> Seq.find (fun i -> 
        let hash = gethash i
        hash.[0..5] = "000000")
    printfn "part 2: %i" part2


    0

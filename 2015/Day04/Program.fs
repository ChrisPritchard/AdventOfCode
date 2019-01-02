
[<EntryPoint>]
let main _ =
    let input = "ckczppom"

    use md5 = System.Security.Cryptography.MD5.Create()
    let gethash i = 
        let text = input + string i
        let bytes = System.Text.Encoding.UTF8.GetBytes text
        md5.ComputeHash bytes

    let part1 = 
        Seq.initInfinite id |> Seq.find (fun i -> 
        let hash = gethash i
        hash.[0..4] = [|0uy;0uy;0uy;0uy;0uy|])
    printfn "part 1: %i" part1

    0

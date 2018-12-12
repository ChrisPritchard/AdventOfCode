open System.IO

[<EntryPoint>]
let main _ =
    
    let lines = File.ReadAllLines "input.txt"
    let lines = [|
        "1, 1"
        "1, 6"
        "8, 3"
        "3, 4"
        "5, 5"
        "8, 9"
    |]

    0
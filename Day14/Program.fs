
open System

let rec extend (list : string) index1 index2 toSkip =
    let num1 = int list.[index1] - int '0'
    let num2 = int list.[index2] - int '0'
    let list = list + string (num1 + num2)
    if list.Length >= toSkip + 10 then
        list.[toSkip..toSkip+9]
    else
        let nextIndex1 = (index1 + 1 + num1) % list.Length
        let nextIndex2 = (index2 + 1 + num2) % list.Length
        extend list nextIndex1 nextIndex2 toSkip


[<EntryPoint>]
let main argv =
    let input = 793061

    printfn "part 1: %s" <| extend "37" 0 1 input

    0

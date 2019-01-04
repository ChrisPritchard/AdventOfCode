
open System

[<EntryPoint>]
let main _ =

    let input = "vzbxkghb"

    let rec increment index (input: string) =
        let next = int input.[index] + 1
        if next > int 'z' then 
            let text = input.[0..index-1] + "a" + input.[index+1..]
            if index = 0 then text
            else increment (index - 1) text
        else
            input.[0..index-1] + (string (char next)) + input.[index+1..]

    let test (input: string) =
        if "iol" |> Seq.exists (fun c -> Seq.contains c input) then false
        else if 
            input 
            |> Seq.windowed 3 
            |> Seq.exists (Array.map int >> fun array -> 
                array.[2] - array.[0] = 2 && array.[1] - array.[0] = 1) 
            |> not then false
        else
            input
            |> Seq.fold (fun (last, count) next -> 
                match last with 
                | None -> Some next, count
                | Some c when c = next && not <| Set.contains c count -> None, Set.add c count
                | Some c -> Some next, count) (None, Set.empty)
            |> snd |> Set.count = 2

    let rec findNext input =
        let next = increment (Seq.length input - 1) input
        if test next then next else findNext next 

    let part1 = findNext input
    printfn "part 1: %s" <| part1

    let part2 = findNext part1
    printfn "part 1: %s" <| part2

    0